test_that("the whole loop runs: lobby, pools, scoring, collapse, finish", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  sid <- fl_create_session(con, label = "test", n_rounds = 3, n_pools = 2,
                           max_harvest = 20, fish_per_player = 10)
  s <- fl_active_session(con)
  expect_equal(s$session_id, sid); expect_equal(s$phase, "lobby")

  ids <- vapply(c("Ada", "Boaz", "Cleo", "Dmitri"), function(n) fl_add_player(con, sid, n)$player_id, numeric(1))
  expect_error(fl_add_player(con, sid, "ada"), "already taken")
  expect_error(fl_add_player(con, sid, "  "), "enter a name")
  expect_error(fl_submit(con, sid, ids[["Ada"]], 1, 5), "not open")

  fl_start_game(con, sid, seed = 3)
  s <- fl_session(con, sid); expect_equal(s$phase, "open"); expect_equal(s$current_round, 1L)
  pools <- fl_pools(con, sid)
  expect_equal(nrow(pools), 2L); expect_equal(pools$n_players, c(2L, 2L))
  expect_equal(pools$start_stock, c(20, 20)); expect_equal(pools$stock, c(20, 20))
  pl <- fl_players(con, sid)
  A <- pl$player_id[pl$pool_id == 1]; B <- pl$player_id[pl$pool_id == 2]
  expect_length(A, 2); expect_length(B, 2)

  # round 1: everyone asks 5, under the stock
  for (id in c(A, B)) fl_submit(con, sid, id, 1, 5, confirm = TRUE)
  rs <- fl_round_status(con, sid)
  expect_equal(rs$n_expected, 4L); expect_equal(rs$n_confirmed, 4L); expect_length(rs$missing, 0)
  expect_error(fl_submit(con, sid, A[1], 1, 21), "0 to 20")
  expect_error(fl_submit(con, sid, A[1], 1, 2.5), "whole number")
  expect_error(fl_submit(con, sid, A[1], 2, 5), "current round")
  r <- fl_score_round(con, sid)
  expect_equal(r$pools_scored, 2L); expect_length(r$no_shows, 0)
  pools <- fl_pools(con, sid)
  expect_equal(pools$stock, c(20, 20))           # 10 remain, double to 20 (cap)
  res <- db_query(con, "SELECT * FROM results WHERE session_id = ?", list(sid))
  expect_equal(nrow(res), 4L); expect_equal(res$catch, rep(5, 4)); expect_equal(res$cumulative_catch, rep(5, 4))
  pr <- db_query(con, "SELECT * FROM pool_rounds WHERE session_id = ? ORDER BY pool_id", list(sid))
  expect_equal(pr$stock_start, c(20, 20)); expect_equal(pr$remaining, c(10, 10)); expect_equal(pr$collapsed, c(0L, 0L))

  # scoring again recomputes from the same starting stock
  fl_score_round(con, sid)
  expect_equal(fl_pools(con, sid)$stock, c(20, 20))
  expect_equal(db_query(con, "SELECT cumulative_catch FROM results WHERE session_id = ?", list(sid))$cumulative_catch, rep(5, 4))

  expect_equal(fl_advance_round(con, sid), "open")
  s <- fl_session(con, sid); expect_equal(s$current_round, 2L)

  # round 2: pool A overfishes and collapses, pool B is careful
  for (id in A) fl_submit(con, sid, id, 2, 20, confirm = TRUE)
  for (id in B) fl_submit(con, sid, id, 2, 2, confirm = TRUE)
  r <- fl_score_round(con, sid)
  expect_equal(r$collapsed, "Pool A")
  pools <- fl_pools(con, sid)
  expect_equal(pools$collapsed_round, c(2L, NA)); expect_equal(pools$stock, c(0, 20))
  resA <- db_query(con, "SELECT catch, cumulative_catch FROM results WHERE session_id = ? AND round = 2 AND pool_id = 1", list(sid))
  expect_equal(resA$catch, c(10, 10)); expect_equal(resA$cumulative_catch, c(15, 15))

  expect_equal(fl_advance_round(con, sid), "open")
  expect_equal(fl_session(con, sid)$current_round, 3L)
  rs <- fl_round_status(con, sid)
  expect_equal(rs$n_expected, 2L)                 # only pool B fishes now
  expect_error(fl_submit(con, sid, A[1], 3, 1), "collapsed")

  # round 3: one B player enters nothing: scored as 0, then carried forward on re-score
  fl_submit(con, sid, B[1], 3, 4, confirm = TRUE)
  r <- fl_score_round(con, sid)
  expect_equal(r$pools_scored, 1L)
  expect_length(r$no_shows, 1)
  rb <- db_query(con, "SELECT player_id, ask, no_submission FROM results WHERE session_id = ? AND round = 3", list(sid))
  expect_equal(nrow(rb), 2L)
  expect_equal(rb$ask[rb$player_id == B[2]], 0); expect_equal(rb$no_submission[rb$player_id == B[2]], 1L)
  r <- fl_score_round(con, sid, carry_forward = TRUE)
  expect_length(r$no_shows, 0)
  rb <- db_query(con, "SELECT player_id, ask FROM results WHERE session_id = ? AND round = 3", list(sid))
  expect_equal(rb$ask[rb$player_id == B[2]], 2)   # last round's ask
  expect_equal(fl_pools(con, sid)$stock[2], 20)   # 20 - 6 = 14, doubles, capped

  expect_equal(fl_advance_round(con, sid), "finished")
  st <- fl_load_state(con, sid)
  expect_equal(st$session$phase, "finished")
  lb <- fl_leaderboard(st)
  expect_equal(nrow(lb), 4L)
  expect_equal(lb$total_catch[lb$player_id == B[1]], 5 + 2 + 4)
  expect_equal(lb$total_catch[lb$player_id == B[2]], 5 + 2 + 2)
  expect_false(any(lb$prize))
  ex <- fl_export_frame(con, sid)
  expect_equal(nrow(ex), 4 + 4 + 2)
  expect_true(all(c("pool_stock_start", "pool_collapsed") %in% names(ex)))

  # the chart data: start, then one point per scored round per pool
  d <- stock_path(st)
  expect_equal(d$stock[d$pool == "Pool A"], c(20, 20, 0))
  expect_equal(d$stock[d$pool == "Pool B"], c(20, 20, 20, 20))
  expect_true(any(d$collapsed))
  g <- plot_stock(st); expect_s3_class(g, "ggplot")
})

test_that("the game ends early when every pool has collapsed", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  sid <- fl_create_session(con, n_rounds = 5, n_pools = 1, fish_per_player = 10)
  ids <- vapply(c("X", "Y"), function(n) fl_add_player(con, sid, n)$player_id, numeric(1))
  fl_start_game(con, sid)
  for (id in ids) fl_submit(con, sid, id, 1, 20, confirm = TRUE)   # 40 asked of 20
  r <- fl_score_round(con, sid)
  expect_equal(r$collapsed, "Pool A")
  expect_equal(db_query(con, "SELECT catch FROM results WHERE session_id = ?", list(sid))$catch, c(10, 10))
  expect_equal(fl_advance_round(con, sid), "finished")
})

test_that("reset returns to the lobby with the roster kept, and history lists sessions", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  sid <- fl_create_session(con, n_rounds = 2, n_pools = 1)
  fl_add_player(con, sid, "P"); fl_add_player(con, sid, "Q")
  fl_start_game(con, sid)
  fl_score_round(con, sid)
  fl_reset_to_lobby(con, sid)
  s <- fl_session(con, sid)
  expect_equal(s$phase, "lobby"); expect_equal(s$current_round, 0L)
  expect_equal(nrow(fl_players(con, sid)), 2L); expect_equal(nrow(fl_pools(con, sid)), 0L)
  expect_true(all(is.na(fl_players(con, sid)$pool_id)))
  h <- fl_list_sessions(con)
  expect_equal(nrow(h), 1L); expect_true(h$is_active[1])
  fl_clear_active(con); expect_null(fl_active_session(con))
  fl_set_active(con, sid); expect_equal(fl_active_session(con)$session_id, sid)
  fl_delete_session(con, sid)
  expect_null(fl_session(con, sid)); expect_null(fl_active_session(con))
})

test_that("more pools than players collapses to one pool per player", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  sid <- fl_create_session(con, n_rounds = 1, n_pools = 5)
  fl_add_player(con, sid, "Solo")
  info <- fl_start_game(con, sid)
  expect_equal(info$n_pools, 1L)
  expect_equal(fl_session(con, sid)$n_pools, 1L)
  expect_equal(fl_pools(con, sid)$start_stock, 10)
})

test_that("the state stamp moves on every change a screen shows", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  sid <- fl_create_session(con, n_rounds = 2, n_pools = 1)
  s0 <- fl_state_stamp(con, sid)
  id <- fl_add_player(con, sid, "Z")$player_id
  s1 <- fl_state_stamp(con, sid); expect_false(identical(s0, s1))
  fl_start_game(con, sid)
  s2 <- fl_state_stamp(con, sid); expect_false(identical(s1, s2))
  fl_submit(con, sid, id, 1, 3)
  s3 <- fl_state_stamp(con, sid); expect_false(identical(s2, s3))
  Sys.sleep(0.01); fl_submit(con, sid, id, 1, 4)
  s4 <- fl_state_stamp(con, sid); expect_false(identical(s3, s4))
  fl_score_round(con, sid)
  s5 <- fl_state_stamp(con, sid); expect_false(identical(s4, s5))
})
