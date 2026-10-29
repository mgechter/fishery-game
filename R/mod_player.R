# Player-facing module: join, ask for a harvest each round, see the result.
# (Structure adapted from Ari Anisfeld's pricing_game, MIT licence.)

player_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "fg-narrow", shiny::uiOutput(ns("screen")))
}

player_server <- function(id, con) shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns

  token <- shiny::reactiveVal(
    shiny::parseQueryString(shiny::isolate(session$clientData$url_search))$t
  )

  state <- shiny::reactivePoll(1000, session,
    checkFunc = function() {
      s <- fl_active_session(con)
      if (is.null(s)) "none" else paste0(s$session_id, "@", fl_state_stamp(con, s$session_id))
    },
    valueFunc = function() {
      s <- fl_active_session(con)
      if (is.null(s)) NULL else fl_load_state(con, s$session_id)
    })

  me <- shiny::reactive({
    st <- state(); tk <- token()
    if (is.null(st) || is.null(tk) || !nzchar(tk)) return(NULL)
    p <- fl_player_by_token(con, tk)
    if (is.null(p) || p$session_id != st$session$session_id) return(NULL)
    p
  })

  my_pool <- shiny::reactive({
    st <- state(); p <- me()
    if (is.null(st) || is.null(p) || is.na(p$pool_id)) return(NULL)
    q <- st$pools[st$pools$pool_id == p$pool_id, , drop = FALSE]
    if (nrow(q)) as.list(q[1, ]) else NULL
  })

  my_submission <- shiny::reactive({
    st <- state(); p <- me(); if (is.null(st) || is.null(p)) return(NULL)
    s <- st$submissions[st$submissions$player_id == p$player_id, , drop = FALSE]
    if (nrow(s)) s else NULL
  })

  my_result <- shiny::reactive({
    st <- state(); p <- me(); if (is.null(st) || is.null(p)) return(NULL)
    r <- st$results[st$results$player_id == p$player_id &
                    st$results$round == st$session$current_round, , drop = FALSE]
    if (nrow(r)) r else NULL
  })

  # A coarse key for the player's screen, so the screen re-renders only when
  # it truly changes. A renderUI that depended on state() directly would
  # rebuild the harvest input on every poll tick and wipe what the player
  # had typed whenever someone else submitted.
  screen_key <- shiny::reactiveVal("init")
  shiny::observe({
    k <- pl_screen_key(state(), me(), my_result())
    if (!identical(k, shiny::isolate(screen_key()))) screen_key(k)
  })

  shiny::observeEvent(input$join, {
    st <- state(); shiny::req(st)
    r <- tryCatch(fl_add_player(con, st$session$session_id, input$name),
                  error = function(e) e)
    if (inherits(r, "error")) {
      shiny::showNotification(conditionMessage(r), type = "error"); return()
    }
    token(r$token)
    shiny::updateQueryString(paste0("?t=", r$token), mode = "replace")
  })

  shiny::observeEvent(input$submit, {
    st <- state(); p <- me(); shiny::req(st, p)
    r <- tryCatch(fl_submit(con, st$session$session_id, p$player_id,
                            st$session$current_round, input$harvest, confirm = TRUE),
                  error = function(e) e)
    if (inherits(r, "error")) shiny::showNotification(conditionMessage(r), type = "error")
    else shiny::showNotification("Submitted", type = "message", duration = 2)
  })

  # Auto-save the box as they type (debounced) so a typed-but-unsent number is
  # never scored as a no-show. Does not confirm.
  draft <- shiny::debounce(shiny::reactive(input$harvest), 900)
  shiny::observeEvent(draft(), {
    st <- state(); pl <- me()
    if (is.null(st) || is.null(pl) || st$session$phase != "open") return()
    h <- suppressWarnings(as.numeric(draft()))
    if (length(h) != 1 || is.na(h) || h < 0 || h > st$session$max_harvest || h != floor(h)) return()
    sub <- shiny::isolate(my_submission())
    if (!is.null(sub) && isTRUE(sub$harvest[1] == h)) return()
    ok <- tryCatch({
      fl_submit(con, st$session$session_id, pl$player_id, st$session$current_round, h, confirm = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (ok) shiny::removeNotification("save-err")
    else shiny::showNotification("Couldn't save just now. Press Submit to be sure.",
                                 id = "save-err", type = "warning", duration = NULL)
  }, ignoreInit = TRUE)

  output$save_status <- shiny::renderText({
    sub <- my_submission()
    if (is.null(sub)) return("Nothing saved yet: type a number.")
    carried <- if (isTRUE(sub$carried[1] == 1)) " (carried from last round)" else ""
    if (isTRUE(sub$confirmed[1] == 1))
      sprintf("Submitted: %s fish%s. You can still change it until the round closes.",
              fmt_fish(sub$harvest[1]), carried)
    else
      sprintf("Saved: %s fish%s. Press Submit when you're sure.", fmt_fish(sub$harvest[1]), carried)
  })

  output$screen <- shiny::renderUI({
    screen_key()                      # the only reactive dependency (see above)
    shiny::isolate({
      pl_screen(ns, state(), me(), my_pool(), my_submission(), my_result())
    })
  })

  # Independent of screen_key so the countdown never rebuilds the input.
  output$timer <- shiny::renderUI({
    shiny::invalidateLater(1000)
    st <- state(); if (is.null(st)) return(NULL)
    countdown_tag(fl_time_left(st$session), size = "3rem",
                  note = "your number is saved as you type")
  })

  # In the last seconds of a timed round, send whatever valid number is in the
  # box so a player who typed but did not press Submit is not scored as 0.
  auto_done <- shiny::reactiveVal(-1L)
  shiny::observe({
    shiny::invalidateLater(1000)
    st <- state(); p <- me()
    if (is.null(st) || is.null(p)) return()
    s <- st$session
    if (s$phase != "open" || auto_done() == s$current_round) return()
    tl <- fl_time_left(s)
    if (is.na(tl) || tl > 5) return()
    h <- suppressWarnings(as.numeric(shiny::isolate(input$harvest)))
    if (length(h) != 1 || is.na(h) || h < 0 || h > s$max_harvest || h != floor(h)) return()
    sub <- shiny::isolate(my_submission())
    if (!is.null(sub) && isTRUE(sub$harvest[1] == h)) { auto_done(s$current_round); return() }
    ok <- tryCatch({ fl_submit(con, s$session_id, p$player_id, s$current_round, h); TRUE },
                   error = function(e) FALSE)
    if (ok) {
      auto_done(s$current_round)
      shiny::showNotification("Time's up: your number was submitted automatically.",
                              type = "message", duration = 4)
    }
  })
})

pl_screen <- function(ns, st, p, pool, sub, res) {
  if (is.null(st))
    return(pl_card("No game is running yet",
      "Keep this page open. It updates by itself when the instructor starts one."))
  if (is.null(p)) return(pl_join(ns, st))
  ph <- st$session$phase
  if (ph == "lobby") return(pl_lobby(p, st))
  if (is.null(pool))
    return(pl_card("You're not in a pool this game",
      "You joined after the game started. Watch the projector; you're in next time."))
  rnd <- st$session$current_round
  if (!is.na(pool$collapsed_round) && pool$collapsed_round < rnd && ph != "finished")
    return(pl_collapsed(p, st, pool))
  switch(ph,
    open     = pl_round(ns, p, st, pool, sub),
    scored   = pl_result(p, st, pool, res),
    finished = pl_finished(p, st, pool),
    pl_card("Standby", "…"))
}

# A coarse identity for the player's screen: changes on phase, round, pool and
# when their result is (re)scored, deliberately not on their own typing or on
# other players' submissions.
pl_screen_key <- function(st, p, res) {
  if (is.null(st)) return("no-game")
  if (is.null(p))  return(paste0("join|", st$session$phase))
  paste(st$session$phase, st$session$current_round, p$pool_id %||% "NA",
        if (st$session$phase == "lobby") nrow(st$players) else "-",
        if (is.null(res)) "-" else res$computed_at[1],
        sep = "|")
}

# --- screens ---------------------------------------------------------------

pl_card <- function(title, body) shiny::div(class = "card",
  shiny::div(class = "card-header", title),
  shiny::div(class = "card-body", shiny::p(body)))

pl_join <- function(ns, st) {
  if (st$session$phase != "lobby")
    return(pl_card("The game has already started",
      "Joining is closed for this game. Watch the projector; you're in next time."))
  shiny::div(class = "card",
    shiny::div(class = "card-header", "The fishery game"),
    shiny::div(class = "card-body",
      shiny::p(sprintf("%d player(s) have joined.", nrow(st$players))),
      shiny::textInput(ns("name"), "Your name", placeholder = "first name and initial"),
      shiny::actionButton(ns("join"), "Join", class = "btn btn-primary")))
}

pl_lobby <- function(p, st) shiny::div(class = "card",
  shiny::div(class = "card-header", "You're in"),
  shiny::div(class = "card-body",
    shiny::div(class = "fg-big", p$display_name),
    shiny::p(class = "fg-muted",
      sprintf("Waiting for the instructor to start. %d player(s) have joined.", nrow(st$players))),
    shiny::p(class = "fg-muted", "Keep this page open: it updates by itself.")))

pl_rules_line <- function(s, pool) {
  sprintf(paste0("If the pool's asks add up to more than the fish in it, each of you gets a share ",
                 "in proportion to your ask. Whatever is left doubles for the next round, up to %s, ",
                 "as long as at least %d fish remain. Below %d the pool collapses and fishing there ends."),
          fmt_fish(pool$start_stock), pool$n_players, pool$n_players)
}

pl_round <- function(ns, p, st, pool, sub) {
  s <- st$session
  last <- if (!is.null(sub)) sub$harvest[1] else NA_real_
  others <- pool$n_players - 1L
  shiny::div(class = "card",
    shiny::div(class = "card-header", round_title(s)),
    shiny::div(class = "card-body",
      shiny::div(style = "text-align:center;margin-bottom:.5rem;", shiny::uiOutput(ns("timer"))),
      shiny::p(shiny::strong(sprintf("You're in %s with %d other fisher%s.",
                                     pool_label(pool$pool_id), others, if (others == 1) "" else "s"))),
      shiny::div(class = "fg-big", sprintf("%s fish in the pool now", fmt_fish(pool$stock))),
      shiny::numericInput(ns("harvest"),
        sprintf("How many fish do you want to harvest? (0 to %d)", s$max_harvest),
        value = if (is.na(last)) NULL else last, min = 0, max = s$max_harvest, step = 1),
      shiny::actionButton(ns("submit"), "Submit", class = "btn btn-primary btn-lg"),
      shiny::p(class = "fg-muted", style = "margin-top:.5rem;",
               shiny::textOutput(ns("save_status"), inline = TRUE)),
      shiny::hr(),
      shiny::p(class = "fg-muted", style = "font-size:.9rem;", pl_rules_line(s, pool))))
}

pl_pool_line <- function(st, pool, rnd) {
  pr <- st$pool_rounds[st$pool_rounds$pool_id == pool$pool_id & st$pool_rounds$round == rnd, , drop = FALSE]
  if (nrow(pr) == 0) return(NULL)
  pr <- pr[1, ]
  lbl <- pool_label(pool$pool_id)
  first <- sprintf("%s asked for %s fish in total, out of %s in the pool: %s were caught and %s remained.",
                   lbl, fmt_fish(pr$total_ask), fmt_fish(pr$stock_start),
                   fmt_fish(pr$harvested), fmt_fish(pr$remaining))
  second <- if (pr$collapsed == 1)
    sprintf("That is below the %d fish the pool needs to survive, so %s has collapsed. No more fishing there.",
            pool$n_players, lbl)
  else sprintf("The survivors reproduced: %s fish for the next round (the pool can't hold more than %s).",
               fmt_fish(pr$stock_next), fmt_fish(pool$start_stock))
  shiny::tagList(shiny::p(first), shiny::p(shiny::strong(second)))
}

pl_result <- function(p, st, pool, r) {
  rnd <- st$session$current_round
  if (is.null(r)) return(pl_card(sprintf("Round %d results", rnd), "Your pool wasn't scored this round."))
  shiny::div(class = "card",
    shiny::div(class = "card-header", sprintf("Round %d results", rnd)),
    shiny::div(class = "card-body",
      if (isTRUE(r$no_submission == 1))
        shiny::p(class = "text-danger", "You didn't enter a number this round, so you asked for 0."),
      stat_block("You asked for", fmt_fish(r$ask)),
      stat_block("You caught", fmt_fish(r$catch)),
      stat_block("Your total so far", fmt_fish(r$cumulative_catch)),
      pl_pool_line(st, pool, rnd),
      shiny::p(class = "fg-muted", "Waiting for the next round…")))
}

pl_collapsed <- function(p, st, pool) {
  mine <- st$results[st$results$player_id == p$player_id, , drop = FALSE]
  total <- if (nrow(mine)) max(mine$cumulative_catch) else 0
  shiny::div(class = "card",
    shiny::div(class = "card-header", sprintf("%s has collapsed", pool_label(pool$pool_id))),
    shiny::div(class = "card-body",
      shiny::p(sprintf("The pool fell below %d fish in round %d, so there is nothing left to catch.",
                       pool$n_players, pool$collapsed_round)),
      stat_block("Your total catch", fmt_fish(total)),
      shiny::p(class = "fg-muted", "Watch the projector: the other pools may still be fishing.")))
}

pl_finished <- function(p, st, pool) {
  mine <- st$results[st$results$player_id == p$player_id, , drop = FALSE]
  mine <- mine[order(mine$round), , drop = FALSE]
  pr   <- st$pool_rounds[st$pool_rounds$pool_id == pool$pool_id, , drop = FALSE]
  after <- vapply(mine$round, function(r) {
    row <- pr[pr$round == r, , drop = FALSE]
    if (nrow(row) == 0) "—" else if (row$collapsed[1] == 1) "collapsed" else fmt_fish(row$stock_next[1])
  }, "")
  tbl <- data.frame(Round = mine$round, Asked = fmt_fish(mine$ask), Caught = fmt_fish(mine$catch),
                    `Pool after` = after, check.names = FALSE)
  total <- if (nrow(mine)) max(mine$cumulative_catch) else 0
  thr <- st$session$prize_threshold
  shiny::div(class = "card",
    shiny::div(class = "card-header", "Game over"),
    shiny::div(class = "card-body",
      shiny::div(class = "fg-big", sprintf("Your total catch: %s fish", fmt_fish(total))),
      if (total >= thr)
        shiny::p(class = "text-success", shiny::strong(
          sprintf("That's %s or more: you've reached the prize threshold.", fmt_fish(thr))))
      else shiny::p(class = "fg-muted", sprintf("The prize threshold was %s.", fmt_fish(thr))),
      html_table(tbl)))
}
