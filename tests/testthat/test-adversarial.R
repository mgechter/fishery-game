# Adversarial audit, written by OpenAI Codex on 2026-10-29 against commit d54c26b
# (its report is in the commit that added this file). DEFECT blocks failed on
# that commit and pass since the fixes of the same day, except D11, skipped as a
# design choice; REGRESSION GUARD blocks record attacks the code withstood.
# No browser or external service is used; module tests run actual Shiny observers.
for (f in c("mod_player.R", "mod_admin.R", "mod_display.R"))
  source(file.path(root, "R", f))

adv_env <- function() {
  e <- new.env(parent = globalenv())
  for (f in c("engine", "db", "game_logic", "ui_helpers", "plots",
              "mod_player", "mod_admin", "mod_display"))
    sys.source(file.path(root, "R", paste0(f, ".R")), envir = e)
  e
}

adv_game <- function(con, n = 2, rounds = 4, pools = 1, e = NULL, ...) {
  if (is.null(e)) e <- environment(fl_create_session)
  sid <- e$fl_create_session(con, n_rounds = rounds, n_pools = pools, ...)
  people <- lapply(seq_len(n), function(i) e$fl_add_player(con, sid, paste0("P", i)))
  e$fl_start_game(con, sid, seed = 17)
  list(sid = sid, ids = vapply(people, `[[`, numeric(1), "player_id"),
       tokens = vapply(people, `[[`, "", "token"))
}

adv_score_data <- function(con, sid) {
  st <- fl_load_state(con, sid)
  list(pools = st$pools, results = st$results[setdiff(names(st$results), "computed_at")],
       pool_rounds = st$pool_rounds[setdiff(names(st$pool_rounds), "computed_at")])
}

test_that("REGRESSION GUARD: exhaustive small pools respect stock and viability boundaries", {
  grid <- expand.grid(a = 0:20, b = 0:20)
  good <- vapply(seq_len(nrow(grid)), function(i) {
    asks <- as.numeric(grid[i, ])
    r <- resolve_pool_round(20, asks, cap = 20, viable = 2)
    remaining <- max(0, 20 - sum(asks))
    expected <- if (sum(asks) <= 20) asks else asks * 20 / sum(asks)
    isTRUE(all.equal(r$catches, expected)) && r$remaining == remaining &&
      identical(r$collapsed, remaining < 2) &&
      r$stock_next == if (remaining < 2) 0 else min(20, 2 * remaining)
  }, logical(1))
  expect_true(all(good))
  for (ask in 0:20) {
    r <- resolve_pool_round(10, ask, 10, 1)
    expect_equal(r$catches, min(ask, 10))
    expect_identical(r$collapsed, ask > 9)
  }
})

test_that("REGRESSION GUARD: prorated floating point dust never revives a pool", {
  set.seed(812)
  good <- replicate(500, {
    n <- sample(2:25, 1)
    asks <- sample(0:20, n, replace = TRUE)
    stock <- max(1, floor(sum(asks) / 2))
    r <- resolve_pool_round(stock, asks, cap = 10 * n, viable = n)
    isTRUE(all.equal(sum(r$catches), as.numeric(stock), tolerance = 1e-12)) &&
      r$remaining == 0 && r$collapsed && r$stock_next == 0
  })
  expect_true(all(good))
})

test_that("REGRESSION GUARD: repeated scores preserve fractional catches and cumulative totals", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 3, rounds = 2)
  for (id in fx$ids) fl_submit(con, fx$sid, id, 1, 3)
  fl_score_round(con, fx$sid); fl_advance_round(con, fx$sid)
  for (i in seq_along(fx$ids)) fl_submit(con, fx$sid, fx$ids[i], 2, c(20, 19, 18)[i])
  fl_score_round(con, fx$sid)
  before <- adv_score_data(con, fx$sid)
  for (i in 1:25) fl_score_round(con, fx$sid)
  expect_equal(adv_score_data(con, fx$sid), before)
  expect_equal(sum(before$results$catch), 39)
  expect_equal(before$results$cumulative_catch[before$results$round == 2],
               3 + c(20, 19, 18) * 30 / 57)
  expect_equal(fl_advance_round(con, fx$sid), "finished")
  expect_equal(fl_session(con, fx$sid)$current_round, 2)
  expect_error(fl_advance_round(con, fx$sid), "score the round first")
})

test_that("REGRESSION GUARD: carry-forward round one and after a no-show both use zero", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1)
  fl_score_round(con, fx$sid, carry_forward = TRUE)
  r <- fl_load_state(con, fx$sid)$results
  expect_equal(r$ask, 0); expect_equal(r$no_submission, 1)
  fl_advance_round(con, fx$sid)
  fl_submit(con, fx$sid, fx$ids[1], 2, 4)
  fl_score_round(con, fx$sid); fl_advance_round(con, fx$sid)
  fl_score_round(con, fx$sid, carry_forward = FALSE); fl_advance_round(con, fx$sid)
  fl_score_round(con, fx$sid, carry_forward = TRUE)
  r <- fl_load_state(con, fx$sid)$results
  expect_equal(r$ask, c(0, 4, 0, 0))
  expect_equal(r$no_submission, c(1, 0, 1, 1))
  expect_equal(fl_advance_round(con, fx$sid), "finished")
})

test_that("REGRESSION GUARD: serialized duplicate joins, late joins and repeated Next are rejected", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  sid <- fl_create_session(con)
  fl_add_player(con, sid, "  Same Name  ")
  expect_error(fl_add_player(con, sid, "same name"), "already taken")
  expect_equal(nrow(fl_players(con, sid)), 1)
  fl_start_game(con, sid)
  expect_error(fl_add_player(con, sid, "Late"), "already started")
  expect_error(fl_start_game(con, sid), "already started")
  fl_score_round(con, sid); fl_score_round(con, sid)
  fl_advance_round(con, sid)
  expect_error(fl_advance_round(con, sid), "score the round first")
  expect_equal(fl_session(con, sid)$current_round, 2)
})

test_that("REGRESSION GUARD: close, reset and delete reject stale submissions and retain the roster on reset", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con)
  fl_submit(con, fx$sid, fx$ids[1], 1, 4)
  fl_score_round(con, fx$sid)
  expect_error(fl_submit(con, fx$sid, fx$ids[1], 1, 5), "not open")
  fl_advance_round(con, fx$sid)
  expect_error(fl_submit(con, fx$sid, fx$ids[1], 1, 5), "current round")
  fl_submit(con, fx$sid, fx$ids[1], 2, 4)
  fl_reset_to_lobby(con, fx$sid)
  st <- fl_load_state(con, fx$sid)
  expect_equal(st$session$phase, "lobby"); expect_equal(st$session$current_round, 0)
  expect_equal(st$players$player_id, fx$ids); expect_equal(st$players$token, fx$tokens)
  expect_true(all(is.na(st$players$pool_id)))
  expect_equal(vapply(st[c("pools", "submissions", "results", "pool_rounds")], nrow, integer(1)),
               c(pools = 0L, submissions = 0L, results = 0L, pool_rounds = 0L))
  expect_error(fl_submit(con, fx$sid, fx$ids[1], 2, 5), "not open")
  expect_equal(fl_player_by_token(con, fx$tokens[1])$player_id, fx$ids[1])
  fl_delete_session(con, fx$sid)
  expect_null(fl_player_by_token(con, fx$tokens[1])); expect_null(fl_active_session(con))
  expect_error(fl_submit(con, fx$sid, fx$ids[1], 1, 5), "no such session")
})

test_that("REGRESSION GUARD: collapsed players cannot submit through the proxy", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 2, pools = 2)
  fl_submit(con, fx$sid, fx$ids[1], 1, 20)
  fl_score_round(con, fx$sid); fl_advance_round(con, fx$sid)
  expect_error(fl_submit(con, fx$sid, fx$ids[1], 2, 1, confirm = TRUE), "collapsed")
  html <- as.character(ad_proxy(identity, fl_load_state(con, fx$sid)))
  expect_false(grepl(sprintf('value="%s"', fx$ids[1]), html, fixed = TRUE))
  expect_true(grepl(sprintf('value="%s"', fx$ids[2]), html, fixed = TRUE))
  shiny::testServer(admin_server, args = list(con = con), {
    session$flushReact()
    session$setInputs(proxy_player = as.character(fx$ids[1]), proxy_harvest = 1, proxy_submit = 1)
    expect_equal(nrow(fl_load_state(con, fx$sid)$submissions), 0)
  })
})

test_that("REGRESSION GUARD: fl_submit validates hostile asks without replacing a saved ask", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1)
  fl_submit(con, fx$sid, fx$ids[1], 1, 5, confirm = TRUE)
  invalid <- list(-1, 0.5, 20.1, 21, 1e300, Inf, -Inf, NaN, NA, NULL,
                  numeric(0), c(1, 2), "fish", "", "2e999", "1; DROP TABLE players")
  for (x in invalid) {
    expect_error(fl_submit(con, fx$sid, fx$ids[1], 1, x))
    expect_equal(fl_load_state(con, fx$sid)$submissions$harvest, 5)
  }
  for (x in list(0, 20, "2e1", "1e1", " 7 ")) {
    expect_no_error(fl_submit(con, fx$sid, fx$ids[1], 1, x))
    expect_equal(fl_load_state(con, fx$sid)$submissions$harvest, as.numeric(x))
  }
  expect_equal(fl_load_state(con, fx$sid)$submissions$confirmed, 1)
})

test_that("REGRESSION GUARD: player numeric input observer rejects hostile values", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1)
  shiny::testServer(player_server, args = list(con = con), {
    token(fx$tokens[1]); session$flushReact()
    session$setInputs(harvest = 5, submit = 1)
    for (i in seq_along(bad <- list(-1, 0.5, 1e300, "oops", NA, Inf))) {
      session$setInputs(harvest = bad[[i]], submit = i + 1)
      session$elapse(1000)
      expect_equal(fl_load_state(con, fx$sid)$submissions$harvest, 5)
    }
    session$setInputs(harvest = "2e1", submit = 99)
    expect_equal(fl_load_state(con, fx$sid)$submissions$harvest, 20)
  })
})

test_that("DEFECT D1: projector chart refreshes after carry-forward re-scoring", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1)
  fl_submit(con, fx$sid, fx$ids[1], 1, 8)
  fl_score_round(con, fx$sid); fl_advance_round(con, fx$sid)
  fl_score_round(con, fx$sid)
  shiny::testServer(display_server, args = list(con = con), {
    session$flushReact()
    expect_equal(shiny::isolate(chart_state())$pools$stock, 8)
    fl_score_round(con, fx$sid, carry_forward = TRUE)
    session$elapse(1000)
    expect_equal(shiny::isolate(state())$pools$stock, 0)
    expect_equal(shiny::isolate(chart_state())$pools$stock, 0)
  })
})

test_that("DEFECT D2: a stale instructor Close must not score the newly opened round", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con)
  shiny::testServer(admin_server, args = list(con = con), {
    session$flushReact()
    expect_equal(shiny::isolate(state())$session$current_round, 1)
    # Another instructor closes round 1 and opens round 2 before this tab polls.
    fl_score_round(con, fx$sid); fl_advance_round(con, fx$sid)
    session$setInputs(score = 1)
    expect_equal(fl_session(con, fx$sid)$phase, "open")
    expect_equal(nrow(fl_load_state(con, fx$sid)$results), length(fx$ids))
  })
})

test_that("DEFECT D3: timer can score round one again after a reset", {
  e <- adv_env(); e$clock <- 1000; e$now_epoch <- function() e$clock
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1, e = e, round_seconds = 10)
  shiny::testServer(e$admin_server, args = list(con = con), {
    session$flushReact()
    e$clock <- 1011; session$elapse(1000)
    expect_equal(e$fl_session(con, fx$sid)$phase, "scored")
    e$fl_reset_to_lobby(con, fx$sid); session$elapse(1000)
    e$fl_start_game(con, fx$sid); session$elapse(1000)
    e$clock <- 1022
    expect_true(e$fl_should_auto_score(e$fl_session(con, fx$sid)))
    session$elapse(2000)
    expect_equal(e$fl_session(con, fx$sid)$phase, "scored")
  })
})

test_that("DEFECT D4: carry-forward policy survives instructor reconnection", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1)
  fl_submit(con, fx$sid, fx$ids[1], 1, 4)
  fl_score_round(con, fx$sid); fl_advance_round(con, fx$sid)
  shiny::testServer(admin_server, args = list(con = con), {
    session$flushReact(); session$setInputs(carry = FALSE); session$setInputs(carry = TRUE)
    expect_equal(fl_session(con, fx$sid)$carry_forward, 1)   # stored in the session row since 2026-10-29
  })
  shiny::testServer(admin_server, args = list(con = con), {
    session$flushReact(); session$setInputs(score = 1)
    r <- fl_load_state(con, fx$sid)$results
    expect_equal(r$ask[r$round == 2], 4)
  })
})

test_that("DEFECT D5: sub-millisecond submission changes invalidate the state stamp", {
  e <- adv_env(); e$clock <- 1000; e$now_epoch <- function() e$clock
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1, e = e)
  e$fl_submit(con, fx$sid, fx$ids[1], 1, 4)
  stamp <- e$fl_state_stamp(con, fx$sid)
  e$clock <- 1000.0001
  e$fl_submit(con, fx$sid, fx$ids[1], 1, 9, confirm = TRUE)
  expect_equal(e$fl_load_state(con, fx$sid)$submissions$harvest, 9)
  expect_false(identical(e$fl_state_stamp(con, fx$sid), stamp))
})

test_that("DEFECT D6: interrupted Start can recover from the SQLite file alone", {
  e <- adv_env()
  path <- tempfile(fileext = ".sqlite")
  con <- e$db_connect(path); e$db_init(con)
  sid <- e$fl_create_session(con, n_pools = 2)
  e$fl_add_player(con, sid, "A"); e$fl_add_player(con, sid, "B")
  original_run <- e$db_run
  e$db_run <- function(con, sql, params = list()) {
    result <- original_run(con, sql, params)
    if (grepl("INSERT INTO pools", sql, fixed = TRUE)) stop("simulated worker loss after commit")
    result
  }
  expect_error(e$fl_start_game(con, sid), "simulated worker loss")
  DBI::dbDisconnect(con)
  con <- e$db_connect(path); on.exit(DBI::dbDisconnect(con)); e$db_init(con)
  e$db_run <- original_run
  expect_equal(e$fl_session(con, sid)$phase, "lobby")
  expect_no_error(e$fl_start_game(con, sid))
})

test_that("DEFECT D7: a pending old-round draft must not become a next-round ask", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1)
  shiny::testServer(player_server, args = list(con = con), {
    token(fx$tokens[1]); session$flushReact()
    session$elapse(600); session$setInputs(harvest = 8)
    # The 900 ms debounce is pending. A slow phone has not sent the replacement
    # round's blank input back by the time the server polls the new round.
    fl_score_round(con, fx$sid); fl_advance_round(con, fx$sid)
    session$elapse(1000)
    expect_equal(nrow(fl_load_state(con, fx$sid)$submissions), 0)
  })
})

test_that("DEFECT D8: reset and restart between polls invalidate the game generation", {
  e <- adv_env(); e$clock <- 1000; e$now_epoch <- function() e$clock
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 4, pools = 2, e = e, round_seconds = 60)
  before <- e$fl_load_state(con, fx$sid)
  stamp <- e$fl_state_stamp(con, fx$sid)
  e$fl_reset_to_lobby(con, fx$sid)
  e$clock <- 1100; e$fl_start_game(con, fx$sid, seed = 42)
  after <- e$fl_load_state(con, fx$sid)
  expect_false(identical(before$players$pool_id, after$players$pool_id))
  expect_equal(after$session$round_started_at - before$session$round_started_at, 100)
  expect_false(identical(stamp, e$fl_state_stamp(con, fx$sid)))
})

test_that("DEFECT D9: a stale expired timer must not score and skip the next round", {
  e <- adv_env(); e$clock <- 1000; e$now_epoch <- function() e$clock
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1, e = e, round_seconds = 10, auto_advance = TRUE)
  ms <- shiny::MockShinySession$new()
  original_history <- e$fl_list_sessions; delayed <- FALSE
  e$fl_list_sessions <- function(con) {
    result <- original_history(con)
    # Model 50 ms spent on the initial history read/render: polling and the
    # timer are independent observers, not guaranteed to have equal deadlines.
    if (!delayed) {
      delayed <<- TRUE
      ms$.__enclos_env__$private$timer$elapse(50)
    }
    result
  }
  shiny::testServer(e$admin_server, args = list(con = con), session = ms, {
    session$flushReact()
    e$clock <- 1011; session$elapse(950) # poll sees expired round 1 at t=1000
    expect_equal(shiny::isolate(state())$session$current_round, 1)
    # The instructor manually closes and advances between poll and timer.
    e$fl_score_round(con, fx$sid); e$fl_advance_round(con, fx$sid)
    session$elapse(50) # timer uses the cached round 1 at t=1050
    expect_equal(e$fl_session(con, fx$sid)$current_round, 2)
    expect_equal(nrow(e$fl_load_state(con, fx$sid)$results), 1)
  })
})

test_that("REGRESSION GUARD: hostile names and labels are stored literally and HTML-escaped", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  payload <- "<script>alert(1)</script>"
  sid <- fl_create_session(con, label = payload, n_pools = 1, n_rounds = 1)
  expect_error(fl_add_player(con, sid, ""), "enter a name")
  expect_error(fl_add_player(con, sid, strrep("x", 200)), "shorter name")
  p <- fl_add_player(con, sid, payload)
  fl_add_player(con, sid, "**bold** [link](javascript:1)")
  expect_equal(fl_session(con, sid)$label, payload)
  expect_equal(fl_player_by_token(con, p$token)$display_name, payload)
  escaped <- function(tag) {
    html <- as.character(tag)
    expect_false(grepl(payload, html, fixed = TRUE))
    expect_true(grepl("&lt;script&gt;", html, fixed = TRUE))
  }
  st <- fl_load_state(con, sid)
  escaped(pl_lobby(fl_player_by_token(con, p$token), st))
  escaped(ad_lobby(identity, st, "http://localhost/"))
  escaped(ad_history(identity, fl_list_sessions(con)))
  fl_start_game(con, sid)
  st <- fl_load_state(con, sid)
  escaped(ad_round(identity, st, "open")); escaped(ad_proxy(identity, st))
  expect_match(as.character(ad_round(identity, st, "open")), "**bold**", fixed = TRUE)
  fl_score_round(con, sid); fl_advance_round(con, sid)
  escaped(ad_finished(identity, fl_load_state(con, sid)))
  shiny::testServer(display_server, args = list(con = con), {
    session$flushReact()
    escaped(output$lb)
  })
})

test_that("REGRESSION GUARD: the actual app server requires the exact admin key value", {
  # Evaluate only app.R's server definition; do not open its production DB.
  code <- parse(file.path(root, "app.R"))
  definition <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
                         identical(x[[2]], as.name("server")), as.list(code))[[1]]
  e <- new.env(parent = asNamespace("shiny"))
  e$ADMIN_KEY <- "s3cret"; e$.con <- NULL
  e$admin_ui <- e$player_ui <- e$display_ui <- function(id) shiny::div(id)
  e$player_server <- e$display_server <- function(...) NULL
  e$called <- FALSE; e$admin_server <- function(...) e$called <- TRUE
  eval(definition, envir = e)
  for (query in c("?role=admin", "?role=admin&key=", "?role=admin&key=S3CRET",
                  "?role=admin&key=s3cre", "?role=admin&key=s3cret%20",
                  "?role=admin&key[]=wrong", "?role=admin&key=s3cret")) {
    e$called <- FALSE
    ms <- shiny::MockShinySession$new(); ms$clientData <- list(url_search = query)
    shiny::testServer(e$server, session = ms, { session$flushReact() })
    expect_identical(e$called, identical(query, "?role=admin&key=s3cret"))
  }
})

test_that("REGRESSION GUARD: each stable phase and every screen rebuild from a reopened database", {
  path <- tempfile(fileext = ".sqlite")
  con <- db_connect(path); db_init(con)
  on.exit(DBI::dbDisconnect(con))
  fx <- list(sid = fl_create_session(con, n_pools = 1, n_rounds = 2,
                                    round_seconds = 3600, auto_advance = FALSE))
  fx$p <- fl_add_player(con, fx$sid, "Reload")
  for (phase in c("lobby", "open", "scored", "finished")) {
    if (phase == "open") {
      fl_start_game(con, fx$sid); fl_submit(con, fx$sid, fx$p$player_id, 1, 5, TRUE)
    }
    if (phase == "scored") fl_score_round(con, fx$sid)
    if (phase == "finished") {
      fl_advance_round(con, fx$sid)
      fl_submit(con, fx$sid, fx$p$player_id, 2, 20, TRUE)
      fl_score_round(con, fx$sid); fl_advance_round(con, fx$sid)
    }
    before <- fl_load_state(con, fx$sid)
    DBI::dbDisconnect(con); con <- db_connect(path); db_init(con)
    expect_equal(fl_load_state(con, fx$sid), before)
    expect_equal(fl_active_session(con)$session_id, fx$sid)
    expect_equal(fl_player_by_token(con, fx$p$token)$player_id, fx$p$player_id)
    shiny::testServer(player_server, args = list(con = con), {
      token(fx$p$token); session$flushReact()
      expect_equal(shiny::isolate(state())$session$phase, phase)
      expect_true(nzchar(output$screen$html))
    })
    shiny::testServer(admin_server, args = list(con = con), {
      session$flushReact()
      expect_equal(shiny::isolate(state())$session$phase, phase)
      expect_true(nzchar(output$screen$html))
    })
    shiny::testServer(display_server, args = list(con = con), {
      session$flushReact()
      expect_equal(shiny::isolate(state())$session$phase, phase)
      expect_true(nzchar(output$screen$html))
    })
  }
})

test_that("REGRESSION GUARD: quantify polling SQL for 25 players and two instructor/display screens", {
  e <- adv_env()
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 25, pools = 2, e = e)
  query_count <- 0L; original_query <- e$db_query
  e$db_query <- function(...) { query_count <<- query_count + 1L; original_query(...) }
  check <- function() {
    s <- e$fl_active_session(con)
    if (is.null(s)) "none" else paste0(s$session_id, "@", e$fl_state_stamp(con, s$session_id))
  }
  for (i in 1:25) check()
  expect_equal(query_count, 150) # six SELECTs per client per check
  query_count <- 0L
  for (i in 1:27) check()
  expect_equal(query_count, 162)
  query_count <- 0L
  s <- e$fl_active_session(con); e$fl_load_state(con, s$session_id)
  expect_equal(query_count, 11) # additional SELECTs on each changed poll
  query_count <- 0L
  e$fl_player_by_token(con, fx$tokens[1])
  expect_equal(query_count, 1) # player's me() invalidates with state()
})

test_that("DEFECT D10: confirming Reset must target the session shown in its dialog", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  first <- adv_game(con, n = 1)
  second <- adv_game(con, n = 1)
  fl_submit(con, second$sid, second$ids[1], 1, 7)
  fl_set_active(con, first$sid)
  shiny::testServer(admin_server, args = list(con = con), {
    session$flushReact(); session$setInputs(reset = 1)
    # Another instructor opens a different session while the dialog is up.
    fl_set_active(con, second$sid); session$elapse(1000)
    session$setInputs(reset_ok = 1)
    expect_equal(fl_session(con, second$sid)$phase, "open")
    expect_equal(fl_load_state(con, second$sid)$submissions$harvest, 7)
  })
})

test_that("REGRESSION GUARD: actual player poll performs six reads idle and eighteen on change", {
  e <- adv_env()
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 25, e = e)
  meter <- new.env(); meter$queries <- 0L; original_query <- e$db_query
  e$db_query <- function(...) { meter$queries <- meter$queries + 1L; original_query(...) }
  shiny::testServer(e$player_server, args = list(con = con), {
    token(fx$tokens[1]); session$flushReact()
    meter$queries <- 0L; session$elapse(1000)
    expect_equal(meter$queries, 6)
    e$fl_submit(con, fx$sid, fx$ids[2], 1, 1)
    meter$queries <- 0L; session$elapse(1000)
    expect_equal(meter$queries, 18)
  })
})

test_that("REGRESSION GUARD: URL tokens restore identity and deleted tokens show the join screen", {
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1)
  ms <- shiny::MockShinySession$new()
  ms$clientData <- list(url_search = paste0("?t=", fx$tokens[1]))
  shiny::testServer(player_server, args = list(con = con), session = ms, {
    session$flushReact()
    expect_equal(shiny::isolate(me())$player_id, fx$ids[1])
    fl_reset_to_lobby(con, fx$sid); session$elapse(1000)
    expect_equal(shiny::isolate(me())$token, fx$tokens[1])
    expect_match(output$screen$html, "You're in", fixed = TRUE)
    fl_delete_session(con, fx$sid)
    fl_create_session(con)
    session$elapse(1000)
    expect_null(shiny::isolate(me()))
    expect_match(output$screen$html, "Your name", fixed = TRUE)
  })
})

test_that("REGRESSION GUARD: a reconnecting instructor recovers the timer deadline and auto-advance", {
  e <- adv_env(); e$clock <- 1000; e$now_epoch <- function() e$clock
  path <- tempfile(fileext = ".sqlite")
  con <- e$db_connect(path); e$db_init(con)
  fx <- adv_game(con, n = 1, e = e, round_seconds = 10, auto_advance = TRUE)
  e$fl_submit(con, fx$sid, fx$ids[1], 1, 4)
  DBI::dbDisconnect(con)
  con <- e$db_connect(path); on.exit(DBI::dbDisconnect(con))
  e$clock <- 1011
  shiny::testServer(e$admin_server, args = list(con = con), {
    session$flushReact(); session$elapse(2000)
    s <- e$fl_session(con, fx$sid)
    expect_equal(s$phase, "open"); expect_equal(s$current_round, 2)
    expect_equal(s$round_started_at, 1011)
    expect_equal(e$fl_load_state(con, fx$sid)$results$catch, 4)
  })
})

test_that("REGRESSION GUARD: retrying an interrupted score uses persisted starting stocks", {
  e <- adv_env()
  path <- tempfile(fileext = ".sqlite")
  con <- e$db_connect(path); e$db_init(con)
  fx <- adv_game(con, n = 4, pools = 2, e = e)
  for (id in fx$ids) e$fl_submit(con, fx$sid, id, 1, 8)
  original_run <- e$db_run
  e$db_run <- function(con, sql, params = list()) {
    result <- original_run(con, sql, params)
    if (grepl("UPDATE pools SET stock", sql, fixed = TRUE)) stop("simulated worker loss after pool")
    result
  }
  expect_error(e$fl_score_round(con, fx$sid), "simulated worker loss")
  DBI::dbDisconnect(con)
  con <- e$db_connect(path); on.exit(DBI::dbDisconnect(con)); e$db_run <- original_run
  expect_no_error(e$fl_score_round(con, fx$sid))
  st <- e$fl_load_state(con, fx$sid)
  expect_equal(st$pools$stock, c(8, 8))
  expect_equal(st$results$catch, rep(8, 4))
  expect_equal(st$results$cumulative_catch, rep(8, 4))
  expect_equal(st$pool_rounds$stock_start, c(20, 20))
})

test_that("DEFECT D11: the timer keeps scoring when only the projector remains connected", {
  skip("by design: the countdown is scored by the instructor tab, which the README says to keep open")
  e <- adv_env(); e$clock <- 1000; e$now_epoch <- function() e$clock
  con <- fresh_db(); on.exit(DBI::dbDisconnect(con))
  fx <- adv_game(con, n = 1, e = e, round_seconds = 10)
  # Closing the instructor tab destroys its observers. A projector connection
  # keeps the worker alive but does not create an admin_server observer.
  shiny::testServer(e$display_server, args = list(con = con), {
    session$flushReact()
    e$clock <- 1011; session$elapse(2000)
    expect_equal(e$fl_session(con, fx$sid)$phase, "scored")
  })
})
