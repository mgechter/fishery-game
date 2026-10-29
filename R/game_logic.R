# Game-loop operations on top of the SQLite store (R/db.R) and the engine
# (R/engine.R). Pure of Shiny: every function takes a connection and returns
# data, so the whole flow runs headless in tests/testthat/test-game-logic.R.
#
# Session phases:  lobby -> open -> scored -> open -> ... -> finished

# --- session lifecycle --------------------------------------------------------

fl_create_session <- function(con, label = NULL, n_rounds = 11, n_pools = 2,
                              max_harvest = 20, fish_per_player = 10, growth = 2,
                              show_rounds = TRUE, prize_threshold = 45,
                              round_seconds = 0, auto_advance = FALSE) {
  n_rounds <- as.integer(n_rounds); n_pools <- as.integer(n_pools)
  max_harvest <- as.integer(max_harvest); fish_per_player <- as.integer(fish_per_player)
  if (is.na(n_rounds) || n_rounds < 1) stop("rounds must be 1 or more", call. = FALSE)
  if (is.na(n_pools) || n_pools < 1) stop("pools must be 1 or more", call. = FALSE)
  if (is.na(max_harvest) || max_harvest < 1) stop("max harvest must be 1 or more", call. = FALSE)
  if (is.na(fish_per_player) || fish_per_player < 1) stop("fish per player must be 1 or more", call. = FALSE)
  label <- trimws(label %||% "")
  db_run(con,
    "INSERT INTO sessions (label, phase, current_round, n_rounds, n_pools, max_harvest,
       fish_per_player, growth, show_rounds, prize_threshold, round_seconds, auto_advance, created_at)
     VALUES (?, 'lobby', 0, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    list(if (nzchar(label)) label else NA_character_, n_rounds, n_pools, max_harvest,
         fish_per_player, as.numeric(growth), as.integer(isTRUE(show_rounds)),
         as.numeric(prize_threshold), max(0L, as.integer(round_seconds %||% 0)),
         as.integer(isTRUE(auto_advance)), now_epoch()))
  sid <- db_last_id(con)
  meta_set(con, "active_session_id", sid)
  sid
}

fl_session <- function(con, session_id) {
  db_one(con, "SELECT * FROM sessions WHERE session_id = ?", list(session_id))
}

fl_active_session <- function(con) {
  sid <- meta_get(con, "active_session_id")
  if (is.null(sid) || !nzchar(sid)) return(NULL)
  fl_session(con, as.integer(sid))
}

fl_list_sessions <- function(con) {
  active <- meta_get(con, "active_session_id")
  active <- if (is.null(active) || !nzchar(active)) -1L else as.integer(active)
  d <- db_query(con, "
    SELECT s.session_id, s.label, s.phase, s.current_round, s.n_rounds, s.n_pools, s.created_at,
           (SELECT COUNT(*) FROM players p WHERE p.session_id = s.session_id) AS n_players,
           (SELECT COUNT(*) FROM results r WHERE r.session_id = s.session_id) AS n_result_rows
    FROM sessions s ORDER BY s.session_id DESC")
  if (nrow(d)) d$is_active <- d$session_id == active
  d
}

fl_set_active <- function(con, session_id) {
  if (is.null(fl_session(con, session_id))) stop("no such session", call. = FALSE)
  meta_set(con, "active_session_id", session_id)
  invisible(TRUE)
}

fl_clear_active <- function(con) meta_set(con, "active_session_id", "")

fl_delete_session <- function(con, session_id) {
  for (tbl in c("submissions", "results", "pool_rounds", "pools", "players", "sessions"))
    db_run(con, sprintf("DELETE FROM %s WHERE session_id = ?", tbl), list(session_id))
  if (identical(meta_get(con, "active_session_id"), as.character(session_id)))
    fl_clear_active(con)
  invisible(TRUE)
}

# Back to the lobby with the roster kept: the pools are dealt again at Start.
fl_reset_to_lobby <- function(con, session_id) {
  for (tbl in c("submissions", "results", "pool_rounds", "pools"))
    db_run(con, sprintf("DELETE FROM %s WHERE session_id = ?", tbl), list(session_id))
  db_run(con, "UPDATE players SET pool_id = NULL WHERE session_id = ?", list(session_id))
  db_run(con,
    "UPDATE sessions SET phase = 'lobby', current_round = 0, round_started_at = NULL
     WHERE session_id = ?", list(session_id))
  invisible(TRUE)
}

# --- players ----------------------------------------------------------------

fl_add_player <- function(con, session_id, display_name) {
  s <- fl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  if (s$phase != "lobby") stop("the game has already started", call. = FALSE)
  name <- trimws(display_name %||% "")
  if (!nzchar(name)) stop("please enter a name", call. = FALSE)
  if (nchar(name) > 40) stop("please use a shorter name", call. = FALSE)
  dup <- db_one(con,
    "SELECT 1 FROM players WHERE session_id = ? AND lower(display_name) = lower(?)",
    list(session_id, name))
  if (!is.null(dup)) stop("that name is already taken: add an initial", call. = FALSE)
  token <- NULL
  for (i in 1:10) {
    cand <- random_token()
    if (is.null(db_one(con, "SELECT 1 FROM players WHERE token = ?", list(cand)))) {
      token <- cand; break
    }
  }
  if (is.null(token)) stop("could not allocate a unique token", call. = FALSE)
  db_run(con,
    "INSERT INTO players (session_id, display_name, token, joined_at) VALUES (?, ?, ?, ?)",
    list(session_id, name, token, now_epoch()))
  list(player_id = db_last_id(con), token = token)
}

fl_player_by_token <- function(con, token) {
  if (is.null(token) || !nzchar(token)) return(NULL)
  db_one(con, "SELECT * FROM players WHERE token = ?", list(token))
}

fl_players <- function(con, session_id) {
  db_query(con, "SELECT * FROM players WHERE session_id = ? ORDER BY joined_at, player_id",
           list(session_id))
}

fl_pools <- function(con, session_id) {
  db_query(con, "SELECT * FROM pools WHERE session_id = ? ORDER BY pool_id", list(session_id))
}

# --- start ----------------------------------------------------------------

fl_start_game <- function(con, session_id, seed = NULL) {
  s <- fl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  if (s$phase != "lobby") stop("the game has already started", call. = FALSE)
  pl <- fl_players(con, session_id)
  if (nrow(pl) < 1) stop("nobody has joined yet", call. = FALSE)
  asg <- assign_pools(pl$player_id, s$n_pools, seed = seed)
  for (i in seq_len(nrow(asg)))
    db_run(con, "UPDATE players SET pool_id = ? WHERE player_id = ?",
           list(asg$pool_id[i], asg$player_id[i]))
  n_pools <- max(asg$pool_id)
  for (k in seq_len(n_pools)) {
    n <- sum(asg$pool_id == k)
    start <- s$fish_per_player * n
    db_run(con,
      "INSERT INTO pools (session_id, pool_id, n_players, start_stock, stock, collapsed_round)
       VALUES (?, ?, ?, ?, ?, NULL)",
      list(session_id, k, n, start, start))
  }
  db_run(con,
    "UPDATE sessions SET n_pools = ?, phase = 'open', current_round = 1, round_started_at = ?
     WHERE session_id = ?",
    list(n_pools, now_epoch(), session_id))
  invisible(list(n_pools = n_pools))
}

# --- timer ------------------------------------------------------------------

fl_set_timer <- function(con, session_id, seconds, auto_advance = NULL) {
  seconds <- max(0L, as.integer(seconds %||% 0L))
  if (is.na(seconds)) seconds <- 0L
  db_run(con, "UPDATE sessions SET round_seconds = ? WHERE session_id = ?",
         list(seconds, session_id))
  if (!is.null(auto_advance))
    db_run(con, "UPDATE sessions SET auto_advance = ? WHERE session_id = ?",
           list(as.integer(isTRUE(auto_advance)), session_id))
  invisible(TRUE)
}

# seconds left in the current round, or NA when there is no timer or no open round
fl_time_left <- function(s) {
  if (is.null(s) || s$phase != "open" || (s$round_seconds %||% 0) <= 0) return(NA_real_)
  s$round_seconds - (now_epoch() - (s$round_started_at %||% now_epoch()))
}

fl_should_auto_score <- function(s) {
  (s$round_seconds %||% 0) > 0 && s$phase == "open" &&
    !is.na(tl <- fl_time_left(s)) && tl <= 0
}

# --- submissions --------------------------------------------------------------

# `confirm = TRUE` marks the round done for this player (sticky: a later
# auto-save with confirm = FALSE does not clear it).
fl_submit <- function(con, session_id, player_id, round, harvest, confirm = FALSE) {
  s <- fl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  if (s$phase != "open") stop("the round is not open", call. = FALSE)
  if (round != s$current_round) stop("that is not the current round", call. = FALSE)
  p <- db_one(con, "SELECT pool_id FROM players WHERE player_id = ? AND session_id = ?",
              list(player_id, session_id))
  if (is.null(p)) stop("unknown player", call. = FALSE)
  if (is.na(p$pool_id)) stop("you are not in a pool this game", call. = FALSE)
  pool <- db_one(con, "SELECT collapsed_round FROM pools WHERE session_id = ? AND pool_id = ?",
                 list(session_id, p$pool_id))
  if (!is.null(pool) && !is.na(pool$collapsed_round) && pool$collapsed_round < s$current_round)
    stop("your pool has collapsed", call. = FALSE)
  harvest <- suppressWarnings(as.numeric(harvest))
  if (length(harvest) != 1 || is.na(harvest)) stop("enter a number of fish", call. = FALSE)
  if (harvest != floor(harvest)) stop("enter a whole number of fish", call. = FALSE)
  if (harvest < 0 || harvest > s$max_harvest)
    stop(sprintf("enter a number from 0 to %d", s$max_harvest), call. = FALSE)
  db_run(con,
    "INSERT INTO submissions (session_id, round, player_id, harvest, confirmed, carried, submitted_at)
     VALUES (?, ?, ?, ?, ?, 0, ?)
     ON CONFLICT(session_id, round, player_id)
     DO UPDATE SET harvest = excluded.harvest, carried = 0,
                   confirmed = MAX(submissions.confirmed, excluded.confirmed),
                   submitted_at = excluded.submitted_at",
    list(session_id, round, player_id, harvest, as.integer(isTRUE(confirm)), now_epoch()))
  invisible(TRUE)
}

# Players expected to submit this round: those in a pool that has not collapsed
# before this round.
fl_expected_players <- function(con, session_id, round) {
  db_query(con,
    "SELECT p.player_id, p.display_name, p.pool_id FROM players p
     JOIN pools q ON q.session_id = p.session_id AND q.pool_id = p.pool_id
     WHERE p.session_id = ? AND (q.collapsed_round IS NULL OR q.collapsed_round >= ?)
     ORDER BY p.pool_id, p.player_id",
    list(session_id, round))
}

fl_round_status <- function(con, session_id) {
  s <- fl_session(con, session_id)
  rnd <- s$current_round
  expected <- fl_expected_players(con, session_id, rnd)
  subs <- db_query(con,
    "SELECT player_id, confirmed FROM submissions WHERE session_id = ? AND round = ?",
    list(session_id, rnd))
  priced    <- expected$player_id %in% subs$player_id
  confirmed <- expected$player_id %in% subs$player_id[subs$confirmed == 1]
  list(
    round       = rnd,
    n_expected  = nrow(expected),
    n_entered   = sum(priced),
    n_confirmed = sum(confirmed),
    unconfirmed = expected$display_name[!confirmed],
    missing     = expected$display_name[!priced]
  )
}

# --- scoring ----------------------------------------------------------------

.prev_ask <- function(con, session_id, player_id, round) {
  v <- db_one(con,
    "SELECT ask, no_submission FROM results WHERE session_id = ? AND player_id = ? AND round = ?",
    list(session_id, player_id, round - 1))
  if (is.null(v) || isTRUE(v$no_submission == 1)) NULL else v$ask
}

# The ask a player is scored at: their submission, or (with carry_forward) last
# round's ask, or 0. Writes the carried-forward row as a side effect.
.effective_ask <- function(con, session_id, player_id, round, carry_forward) {
  sub <- db_one(con,
    "SELECT harvest FROM submissions WHERE session_id = ? AND round = ? AND player_id = ?",
    list(session_id, round, player_id))
  if (!is.null(sub)) return(list(ask = sub$harvest, missing = FALSE))
  if (isTRUE(carry_forward)) {
    pp <- .prev_ask(con, session_id, player_id, round)
    if (!is.null(pp)) {
      db_run(con,
        "INSERT INTO submissions (session_id, round, player_id, harvest, confirmed, carried, submitted_at)
         VALUES (?, ?, ?, ?, 0, 1, ?)
         ON CONFLICT(session_id, round, player_id) DO NOTHING",
        list(session_id, round, player_id, pp, now_epoch()))
      return(list(ask = pp, missing = FALSE))
    }
  }
  list(ask = 0, missing = TRUE)
}

.write_result <- function(con, session_id, round, player_id, pool_id, ask, catch, missing) {
  prior <- db_one(con,
    "SELECT COALESCE(SUM(catch), 0) AS s FROM results
     WHERE session_id = ? AND player_id = ? AND round < ?",
    list(session_id, player_id, round))$s
  db_run(con,
    "INSERT INTO results
       (session_id, round, player_id, pool_id, ask, catch, cumulative_catch, no_submission, computed_at)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
     ON CONFLICT(session_id, round, player_id) DO UPDATE SET
       pool_id = excluded.pool_id, ask = excluded.ask, catch = excluded.catch,
       cumulative_catch = excluded.cumulative_catch, no_submission = excluded.no_submission,
       computed_at = excluded.computed_at",
    list(session_id, round, player_id, pool_id, ask, catch, prior + catch,
         as.integer(missing), now_epoch()))
}

# Score the current round for every pool still alive. A missing ask counts as 0
# unless carry_forward reuses the player's previous ask. Scoring again in the
# same round (phase 'scored') recomputes from the stock the round started with.
fl_score_round <- function(con, session_id, carry_forward = FALSE) {
  s <- fl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  if (!s$phase %in% c("open", "scored")) stop("no round is open", call. = FALSE)
  rnd <- s$current_round
  pools <- fl_pools(con, session_id)
  no_shows <- character(0); scored <- 0L; collapsed_now <- character(0)

  for (i in seq_len(nrow(pools))) {
    pool <- pools[i, ]
    if (!is.na(pool$collapsed_round) && pool$collapsed_round < rnd) next
    prev <- db_one(con,
      "SELECT stock_start FROM pool_rounds WHERE session_id = ? AND round = ? AND pool_id = ?",
      list(session_id, rnd, pool$pool_id))
    stock_start <- if (!is.null(prev)) prev$stock_start else pool$stock
    members <- db_query(con,
      "SELECT player_id, display_name FROM players WHERE session_id = ? AND pool_id = ?
       ORDER BY player_id", list(session_id, pool$pool_id))
    asks <- numeric(nrow(members)); missing <- logical(nrow(members))
    for (j in seq_len(nrow(members))) {
      ep <- .effective_ask(con, session_id, members$player_id[j], rnd, carry_forward)
      asks[j] <- ep$ask; missing[j] <- ep$missing
      if (ep$missing) no_shows <- c(no_shows, members$display_name[j])
    }
    r <- resolve_pool_round(stock_start, asks, cap = pool$start_stock,
                            viable = pool$n_players, growth = s$growth)
    for (j in seq_len(nrow(members)))
      .write_result(con, session_id, rnd, members$player_id[j], pool$pool_id,
                    asks[j], r$catches[j], missing[j])
    db_run(con,
      "INSERT INTO pool_rounds
         (session_id, round, pool_id, stock_start, total_ask, harvested, remaining, stock_next, collapsed, computed_at)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT(session_id, round, pool_id) DO UPDATE SET
         stock_start = excluded.stock_start, total_ask = excluded.total_ask,
         harvested = excluded.harvested, remaining = excluded.remaining,
         stock_next = excluded.stock_next, collapsed = excluded.collapsed,
         computed_at = excluded.computed_at",
      list(session_id, rnd, pool$pool_id, stock_start, r$total_ask, r$harvested,
           r$remaining, r$stock_next, as.integer(r$collapsed), now_epoch()))
    db_run(con,
      "UPDATE pools SET stock = ?, collapsed_round = ? WHERE session_id = ? AND pool_id = ?",
      list(r$stock_next, if (r$collapsed) rnd else NA_integer_, session_id, pool$pool_id))
    if (r$collapsed) collapsed_now <- c(collapsed_now, pool_label(pool$pool_id))
    scored <- scored + 1L
  }
  db_run(con, "UPDATE sessions SET phase = 'scored' WHERE session_id = ?", list(session_id))
  invisible(list(pools_scored = scored, no_shows = unique(no_shows), collapsed = collapsed_now))
}

fl_advance_round <- function(con, session_id) {
  s <- fl_session(con, session_id)
  if (is.null(s)) stop("no such session", call. = FALSE)
  if (s$phase != "scored") stop("score the round first", call. = FALSE)
  alive <- db_query(con,
    "SELECT COUNT(*) AS n FROM pools WHERE session_id = ? AND collapsed_round IS NULL",
    list(session_id))$n
  if (s$current_round >= s$n_rounds || alive == 0) {
    db_run(con, "UPDATE sessions SET phase = 'finished' WHERE session_id = ?", list(session_id))
    return(invisible("finished"))
  }
  db_run(con,
    "UPDATE sessions SET phase = 'open', current_round = current_round + 1,
            round_started_at = ? WHERE session_id = ?",
    list(now_epoch(), session_id))
  invisible("open")
}

# --- read models for the UI -------------------------------------------------

# Changes whenever anything a screen shows could have changed; the pollers
# compare it once a second and reload only then.
fl_state_stamp <- function(con, session_id) {
  s <- fl_session(con, session_id)
  if (is.null(s)) return("none")
  n_pl <- db_query(con, "SELECT COUNT(*) n FROM players WHERE session_id = ?", list(session_id))$n
  sub <- db_query(con,
    "SELECT COUNT(*) n, COALESCE(MAX(submitted_at), 0) t
       FROM submissions WHERE session_id = ? AND round = ?",
    list(session_id, s$current_round))
  res <- db_query(con,
    "SELECT COUNT(*) n, COALESCE(MAX(computed_at), 0) t FROM results WHERE session_id = ?",
    list(session_id))
  paste(s$phase, s$current_round, n_pl,
        sub$n, sprintf("%.3f", sub$t), res$n, sprintf("%.3f", res$t),
        s$round_seconds %||% 0, s$auto_advance %||% 0, sep = "|")
}

fl_load_state <- function(con, session_id) {
  s <- fl_session(con, session_id)
  if (is.null(s)) return(NULL)
  list(
    session      = s,
    players      = fl_players(con, session_id),
    pools        = fl_pools(con, session_id),
    round_status = fl_round_status(con, session_id),
    submissions  = db_query(con,
      "SELECT * FROM submissions WHERE session_id = ? AND round = ?",
      list(session_id, s$current_round)),
    results      = db_query(con,
      "SELECT r.*, p.display_name FROM results r
       JOIN players p ON p.player_id = r.player_id
       WHERE r.session_id = ? ORDER BY r.round, r.pool_id, r.player_id",
      list(session_id)),
    pool_rounds  = db_query(con,
      "SELECT * FROM pool_rounds WHERE session_id = ? ORDER BY pool_id, round",
      list(session_id))
  )
}

# Total catch per player, best first, with the prize flag.
fl_leaderboard <- function(st) {
  pl <- st$players[!is.na(st$players$pool_id), , drop = FALSE]
  if (nrow(pl) == 0) return(data.frame())
  tot <- vapply(pl$player_id, function(id) {
    v <- st$results$catch[st$results$player_id == id]
    if (length(v)) sum(v) else 0
  }, numeric(1))
  d <- data.frame(player_id = pl$player_id, display_name = pl$display_name,
                  pool_id = pl$pool_id, total_catch = tot, stringsAsFactors = FALSE)
  d$prize <- d$total_catch >= st$session$prize_threshold
  d[order(-d$total_catch, d$display_name), ]
}

# Long results plus the pool bookkeeping, for CSV export.
fl_export_frame <- function(con, session_id) {
  db_query(con,
    "SELECT r.round, r.pool_id, p.display_name, r.ask, r.catch, r.cumulative_catch,
            r.no_submission, s.confirmed, s.carried,
            q.stock_start AS pool_stock_start, q.total_ask AS pool_total_ask,
            q.harvested AS pool_harvested, q.remaining AS pool_remaining,
            q.stock_next AS pool_stock_next, q.collapsed AS pool_collapsed
     FROM results r
     JOIN players p ON p.player_id = r.player_id
     LEFT JOIN submissions s
            ON s.session_id = r.session_id AND s.round = r.round AND s.player_id = r.player_id
     LEFT JOIN pool_rounds q
            ON q.session_id = r.session_id AND q.round = r.round AND q.pool_id = r.pool_id
     WHERE r.session_id = ?
     ORDER BY r.round, r.pool_id, p.display_name",
    list(session_id))
}
