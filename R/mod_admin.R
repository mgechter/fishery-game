# Instructor module: create a session, run the round loop, monitor, export.
# (Structure adapted from Ari Anisfeld's pricing_game, MIT licence.)

admin_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "fg-wide", shiny::uiOutput(ns("screen")), shiny::uiOutput(ns("proxy")))
}

admin_server <- function(id, con) shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns

  state <- shiny::reactivePoll(1000, session,
    checkFunc = function() {
      s <- fl_active_session(con)
      if (is.null(s)) "none" else paste0(s$session_id, "@", fl_state_stamp(con, s$session_id))
    },
    valueFunc = function() {
      s <- fl_active_session(con)
      if (is.null(s)) NULL else fl_load_state(con, s$session_id)
    })

  sid <- function() state()$session$session_id
  carry_rv <- shiny::reactiveVal(FALSE)   # persists across screen re-renders
  shiny::observeEvent(input$carry, carry_rv(isTRUE(input$carry)), ignoreInit = TRUE)

  guard <- function(expr) tryCatch(expr, error = function(e)
    shiny::showNotification(conditionMessage(e), type = "error"))

  shiny::observeEvent(input$create, {
    guard(fl_create_session(con,
      label = input$new_label, n_rounds = input$new_rounds, n_pools = input$new_pools,
      max_harvest = input$new_max, fish_per_player = input$new_fpp,
      show_rounds = isTRUE(input$new_show), prize_threshold = input$new_prize,
      round_seconds = input$new_timer, auto_advance = isTRUE(input$new_auto)))
  })

  shiny::observeEvent(input$start, {
    st <- state(); shiny::req(st)
    guard({
      info <- fl_start_game(con, sid())
      shiny::showNotification(sprintf("Started: %d pool(s), round 1 is open.", info$n_pools),
                              type = "message", duration = 4)
    })
  })

  score_now <- function(carry) {
    r <- tryCatch(fl_score_round(con, sid(), carry_forward = carry), error = function(e) e)
    if (inherits(r, "error")) { shiny::showNotification(conditionMessage(r), type = "error"); return() }
    msg <- sprintf("Scored %d pool(s).", r$pools_scored)
    if (length(r$collapsed)) msg <- paste(msg, sprintf("Collapsed: %s.", paste(r$collapsed, collapse = ", ")))
    if (length(r$no_shows))
      msg <- paste(msg, sprintf("No number from %s (counted as 0; re-score with carry-forward if you prefer).",
                                paste(r$no_shows, collapse = ", ")))
    shiny::showNotification(msg, type = if (length(r$no_shows)) "warning" else "message",
                            duration = if (length(r$no_shows)) 9 else 4)
  }
  shiny::observeEvent(input$score,      { st <- state(); shiny::req(st); score_now(carry_rv()) })
  shiny::observeEvent(input$rescore_cf, { st <- state(); shiny::req(st); score_now(TRUE) })
  shiny::observeEvent(input$advance,    { st <- state(); shiny::req(st); guard(fl_advance_round(con, sid())) })

  shiny::observeEvent(input$reset, {
    st <- state(); shiny::req(st)
    shiny::showModal(shiny::modalDialog(
      title = "Reset this game to the lobby?",
      shiny::p("This deletes every ask and result in this session. The player list is kept, ",
               "and the pools are dealt again when you press Start. This cannot be undone."),
      footer = shiny::tagList(shiny::modalButton("Cancel"),
        shiny::actionButton(ns("reset_ok"), "Reset: delete this session's rounds",
                            class = "btn btn-danger"))))
  })
  shiny::observeEvent(input$reset_ok, {
    st <- state(); shiny::req(st)
    guard({ fl_reset_to_lobby(con, sid()); shiny::showNotification("Back in the lobby.", type = "message") })
    shiny::removeModal()
  })

  shiny::observeEvent(input$newsession, fl_clear_active(con))

  # Enter an ask on a player's behalf (phone trouble). Its inputs live outside
  # the main screen and re-render only when the round or the roster changes,
  # so a student's submission cannot wipe what the instructor is typing.
  proxy_key <- shiny::reactiveVal("")
  shiny::observe({
    st <- state()
    k <- if (is.null(st)) "none" else
      paste(st$session$session_id, st$session$phase, st$session$current_round, nrow(st$players))
    if (!identical(k, shiny::isolate(proxy_key()))) proxy_key(k)
  })
  output$proxy <- shiny::renderUI({
    proxy_key()
    st <- shiny::isolate(state())
    if (is.null(st) || st$session$phase != "open" || !identical(view_rv(), "run")) return(NULL)
    ad_proxy(ns, st)
  })
  shiny::observeEvent(input$proxy_submit, {
    st <- state(); shiny::req(st, st$session$phase == "open")
    pid <- suppressWarnings(as.integer(input$proxy_player))
    if (is.na(pid)) { shiny::showNotification("Pick a player.", type = "error"); return() }
    guard({
      fl_submit(con, sid(), pid, st$session$current_round, input$proxy_harvest, confirm = TRUE)
      who <- st$players$display_name[st$players$player_id == pid]
      shiny::showNotification(sprintf("Entered %s for %s.", fmt_fish(input$proxy_harvest), who),
                              type = "message", duration = 3)
    })
  })

  output$dl <- shiny::downloadHandler(
    filename = function() sprintf("fishery_session_%s.csv", sid() %||% "x"),
    content  = function(file) {
      st <- state(); shiny::req(st)
      utils::write.csv(fl_export_frame(con, sid()), file, row.names = FALSE)
    })

  # ---- history ---------------------------------------------------------------
  view_rv   <- shiny::reactiveVal("run")        # "run" | "history"
  hist_tick <- shiny::reactiveVal(0)
  bump      <- function() hist_tick(hist_tick() + 1)
  del_target <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$nav_run,     view_rv("run"))
  shiny::observeEvent(input$nav_history, view_rv("history"))
  shiny::observeEvent(input$create,      bump())
  shiny::observeEvent(input$newsession,  bump())
  shiny::observeEvent(input$advance,     bump())

  bound <- shiny::reactiveVal(character(0))
  shiny::observe({
    hist_tick()
    sess <- fl_list_sessions(con)
    for (k in seq_len(nrow(sess))) {
      i <- as.character(sess$session_id[k])
      if (i %in% bound()) next
      local({
        sc <- i
        shiny::observeEvent(input[[paste0("open_", sc)]], {
          guard({ fl_set_active(con, as.integer(sc)); view_rv("run"); bump() })
        }, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("del_", sc)]], {
          del_target(as.integer(sc))
          shiny::showModal(shiny::modalDialog(title = paste("Delete session", sc, "?"),
            shiny::p("This removes the session and all of its players, asks and results."),
            footer = shiny::tagList(shiny::modalButton("Cancel"),
              shiny::actionButton(ns("del_ok"), "Delete permanently", class = "btn btn-danger"))))
        }, ignoreInit = TRUE)
        output[[paste0("dl_", sc)]] <- shiny::downloadHandler(
          filename = function() sprintf("fishery_session_%s.csv", sc),
          content  = function(file)
            utils::write.csv(fl_export_frame(con, as.integer(sc)), file, row.names = FALSE))
      })
      bound(c(bound(), i))
    }
  })
  shiny::observeEvent(input$del_ok, {
    guard(fl_delete_session(con, del_target()))
    shiny::removeModal(); bump()
    shiny::showNotification("Session deleted.", type = "message")
  })

  # ---- round timer -----------------------------------------------------------
  output$round_timer <- shiny::renderUI({
    shiny::invalidateLater(1000)
    st <- state(); if (is.null(st)) return(NULL)
    countdown_tag(fl_time_left(st$session), size = "3rem",
      note = if (isTRUE((st$session$auto_advance %||% 0) == 1))
        "scores at 0:00, then opens the next round" else "scores at 0:00; you press Next round")
  })
  shiny::observeEvent(list(input$timer_now, input$auto_now), {
    st <- state(); shiny::req(st, st$session$phase %in% c("open", "scored"))
    guard(fl_set_timer(con, sid(), input$timer_now %||% 0, auto_advance = isTRUE(input$auto_now)))
  }, ignoreInit = TRUE)

  last_auto <- shiny::reactiveVal(-1L)
  shiny::observe({
    shiny::invalidateLater(1000)
    st <- shiny::isolate(state()); if (is.null(st)) return()
    s <- st$session
    if (fl_should_auto_score(s) && last_auto() != s$current_round) {
      last_auto(s$current_round)
      guard({
        fl_score_round(con, sid(), carry_forward = carry_rv())
        if (isTRUE((s$auto_advance %||% 0) == 1)) {
          fl_advance_round(con, sid())
          shiny::showNotification(sprintf("Timer expired: round %d scored, moving on.", s$current_round),
                                  type = "message")
        } else {
          shiny::showNotification(sprintf("Timer expired: round %d scored. Press Next round when ready.",
                                          s$current_round), type = "message", duration = 6)
        }
      })
    }
  })

  output$screen <- shiny::renderUI({
    hist_tick()
    st <- state()
    nav <- ad_nav(ns, view_rv())
    if (identical(view_rv(), "history"))
      return(shiny::tagList(nav, ad_history(ns, fl_list_sessions(con))))
    body <- if (is.null(st)) ad_create(ns)
      else switch(st$session$phase,
        lobby    = ad_lobby(ns, st, base_url(session)),
        open     = ad_round(ns, st, "open", carry_rv()),
        scored   = ad_round(ns, st, "scored", carry_rv()),
        finished = ad_finished(ns, st),
        ad_create(ns))
    shiny::tagList(nav, body)
  })
})

# --- screens ---------------------------------------------------------------

ad_nav <- function(ns, view) {
  lk <- function(id, lbl, on) shiny::actionLink(ns(id), lbl,
    style = if (on) "font-weight:600; text-decoration:none;" else "text-decoration:none;")
  shiny::div(style = "margin-bottom:1rem;",
    lk("nav_run", "Run", view == "run"), shiny::HTML("&nbsp;&nbsp;|&nbsp;&nbsp;"),
    lk("nav_history", "History", view == "history"))
}

ad_create <- function(ns) shiny::div(class = "card",
  shiny::div(class = "card-header", "Start a new session"),
  shiny::div(class = "card-body",
    shiny::textInput(ns("new_label"), "Label (optional)", placeholder = "e.g. Section 002, Oct 29"),
    shiny::fluidRow(
      shiny::column(3, shiny::numericInput(ns("new_pools"), "Pools", value = 2, min = 1, step = 1)),
      shiny::column(3, shiny::numericInput(ns("new_rounds"), "Rounds", value = 11, min = 1, step = 1)),
      shiny::column(3, shiny::numericInput(ns("new_max"), "Max harvest per round", value = 20, min = 1, step = 1)),
      shiny::column(3, shiny::numericInput(ns("new_fpp"), "Starting fish per player", value = 10, min = 1, step = 1))),
    shiny::fluidRow(
      shiny::column(3, shiny::numericInput(ns("new_prize"), "Prize threshold (total catch)", value = 45, min = 0, step = 1)),
      shiny::column(3, shiny::numericInput(ns("new_timer"), "Round timer (seconds, 0 = off)", value = 0, min = 0, step = 15)),
      shiny::column(3, shiny::div(style = "margin-top:1.9rem;",
        shiny::checkboxInput(ns("new_auto"), "After a timed round scores, open the next one automatically", FALSE))),
      shiny::column(3, shiny::div(style = "margin-top:1.9rem;",
        shiny::checkboxInput(ns("new_show"), "Players see how many rounds there are", TRUE)))),
    shiny::p(class = "fg-muted", style = "font-size:.9rem;",
      "Players are dealt into the pools at random when you press Start. A pool starts with ",
      "(starting fish per player) × (players in it) fish, which is also its cap; survivors double ",
      "each round; below one fish per player the pool collapses."),
    shiny::actionButton(ns("create"), "Create session", class = "btn btn-primary"),
    shiny::p(class = "fg-muted", style = "margin-top:.75rem;",
      "Projector view: add ", shiny::tags$code("?mode=display"), " to the URL. ",
      "Sessions are kept: see History.")))

ad_history <- function(ns, sess) {
  if (nrow(sess) == 0)
    return(shiny::div(class = "card", shiny::div(class = "card-body", "No sessions yet.")))
  row_ui <- function(s) {
    sc <- as.character(s$session_id)
    status <- if (s$phase %in% c("lobby", "finished")) s$phase
              else sprintf("%s · round %d/%d", s$phase, s$current_round, s$n_rounds)
    shiny::tags$tr(
      shiny::tags$td(sc, if (isTRUE(s$is_active)) shiny::tags$span(class = "badge bg-primary", "active")),
      shiny::tags$td(if (is.na(s$label)) shiny::tags$em(class = "fg-muted", "(unnamed)") else s$label),
      shiny::tags$td(status),
      shiny::tags$td(s$n_players),
      shiny::tags$td(s$n_result_rows),
      shiny::tags$td(fmt_time(s$created_at)),
      shiny::tags$td(style = "white-space:nowrap;",
        shiny::actionButton(ns(paste0("open_", sc)), "Open", class = "btn btn-sm btn-outline-primary"), " ",
        shiny::downloadButton(ns(paste0("dl_", sc)), "CSV", class = "btn btn-sm btn-outline-secondary"), " ",
        shiny::actionButton(ns(paste0("del_", sc)), "Delete", class = "btn btn-sm btn-link text-danger")))
  }
  shiny::div(class = "card",
    shiny::div(class = "card-header", "Session history"),
    shiny::div(class = "card-body",
      shiny::tags$table(class = "table table-sm",
        shiny::tags$thead(shiny::tags$tr(lapply(
          c("#", "Label", "Status", "Players", "Result rows", "Created", ""), shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(sess)), function(i) row_ui(sess[i, ]))))))
}

ad_lobby <- function(ns, st, url) {
  n <- nrow(st$players)
  start_btn <- if (n >= 1)
    shiny::actionButton(ns("start"), sprintf("Start game with %d player(s)", n), class = "btn btn-primary")
  else shiny::tags$button("Start game (nobody has joined yet)", class = "btn btn-primary", disabled = NA)
  shiny::div(class = "card",
    shiny::div(class = "card-header",
      sprintf("Session #%d · lobby · %d pool(s), %d rounds", st$session$session_id,
              st$session$n_pools, st$session$n_rounds)),
    shiny::div(class = "card-body",
      shiny::p(shiny::strong("Student link: "), shiny::tags$code(url)),
      shiny::p(class = "fg-muted", "Students open that link, enter a name, and wait."),
      html_table(data.frame(Player = st$players$display_name, Joined = fmt_time(st$players$joined_at))),
      start_btn, " ",
      shiny::actionButton(ns("newsession"), "Discard and set up another", class = "btn btn-link")))
}

ad_pool_table <- function(st) {
  s <- st$session; rnd <- s$current_round
  pools <- st$pools
  if (nrow(pools) == 0) return(NULL)
  rows <- lapply(seq_len(nrow(pools)), function(i) {
    p <- pools[i, ]
    members <- st$players$player_id[!is.na(st$players$pool_id) & st$players$pool_id == p$pool_id]
    pr <- st$pool_rounds[st$pool_rounds$pool_id == p$pool_id & st$pool_rounds$round == rnd, , drop = FALSE]
    scored <- nrow(pr) > 0
    sub <- st$submissions[st$submissions$player_id %in% members, , drop = FALSE]
    dead_before <- !is.na(p$collapsed_round) && p$collapsed_round < rnd
    data.frame(
      Pool = pool_label(p$pool_id),
      Fishers = p$n_players,
      `Fish at start of round` = if (dead_before) "—" else if (scored) fmt_fish(pr$stock_start[1]) else fmt_fish(p$stock),
      `Asked so far` = if (dead_before) "—" else fmt_fish(sum(sub$harvest)),
      Submitted = if (dead_before) "—" else sprintf("%d of %d", sum(sub$confirmed == 1), length(members)),
      `After this round` = if (!scored) "" else if (pr$collapsed[1] == 1)
        sprintf("%s caught, %s left: collapsed", fmt_fish(pr$harvested[1]), fmt_fish(pr$remaining[1]))
        else sprintf("%s caught, %s left, grows to %s", fmt_fish(pr$harvested[1]),
                     fmt_fish(pr$remaining[1]), fmt_fish(pr$stock_next[1])),
      Status = pool_status(p),
      check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

ad_player_table <- function(st) {
  p <- st$players[!is.na(st$players$pool_id), , drop = FALSE]
  if (nrow(p) == 0) return(NULL)
  p <- p[order(p$pool_id, p$display_name), ]
  sub <- st$submissions
  rnd <- st$session$current_round
  res <- st$results[st$results$round == rnd, , drop = FALSE]
  ask_of <- function(id) {
    row <- sub[sub$player_id == id, , drop = FALSE]
    if (nrow(row)) return(paste0(fmt_fish(row$harvest[1]), if (isTRUE(row$carried[1] == 1)) " (carried)" else ""))
    rr <- res[res$player_id == id, , drop = FALSE]
    if (nrow(rr) && isTRUE(rr$no_submission[1] == 1)) return("(none: 0)")
    "—"
  }
  done_of <- function(id) {
    row <- sub[sub$player_id == id, , drop = FALSE]
    if (!nrow(row)) "" else if (isTRUE(row$confirmed[1] == 1)) "✓" else "…"
  }
  catch_of <- function(id) { v <- res$catch[res$player_id == id]; if (length(v)) fmt_fish(v) else "" }
  total_of <- function(id) {
    v <- st$results$cumulative_catch[st$results$player_id == id]
    if (length(v)) fmt_fish(max(v)) else "0"
  }
  data.frame(
    Pool   = pool_label(p$pool_id),
    Player = p$display_name,
    Asked  = vapply(p$player_id, ask_of, ""),
    Done   = vapply(p$player_id, done_of, ""),
    Caught = vapply(p$player_id, catch_of, ""),
    Total  = vapply(p$player_id, total_of, ""),
    check.names = FALSE, stringsAsFactors = FALSE)
}

ad_round <- function(ns, st, phase, carry = FALSE) {
  s <- st$session; rs <- st$round_status
  timer_row <- shiny::fluidRow(
    shiny::column(6, shiny::div(style = "text-align:center;", shiny::uiOutput(ns("round_timer")))),
    shiny::column(6,
      shiny::numericInput(ns("timer_now"), "Round timer (seconds, 0 = off)",
        value = s$round_seconds %||% 0, min = 0, step = 15),
      shiny::checkboxInput(ns("auto_now"), "After a timed round scores, open the next one automatically",
        value = isTRUE((s$auto_advance %||% 0) == 1))))
  round_res <- st$results[st$results$round == s$current_round, , drop = FALSE]
  had_noshow <- any(round_res$no_submission == 1)
  alive <- sum(is.na(st$pools$collapsed_round))
  last_round <- s$current_round >= s$n_rounds || alive == 0
  controls <- if (phase == "open")
    shiny::tagList(
      shiny::checkboxInput(ns("carry"),
        "Carry forward last round's ask for anyone who enters nothing (otherwise they ask for 0)",
        value = carry),
      shiny::actionButton(ns("score"), "Close and score the round", class = "btn btn-primary btn-lg"))
  else
    shiny::tagList(
      shiny::actionButton(ns("advance"), if (last_round) "Finish game" else "Next round",
                          class = "btn btn-primary btn-lg"),
      if (had_noshow) shiny::div(style = "margin-top:.75rem;",
        shiny::actionButton(ns("rescore_cf"), "Re-score, carrying the no-shows' last ask forward",
                            class = "btn btn-outline-secondary")))
  shiny::div(class = "card",
    shiny::div(class = "card-header",
      sprintf("Session #%d%s", s$session_id, if (is.na(s$label)) "" else paste0(" · ", s$label))),
    shiny::div(class = "card-body",
      shiny::div(class = "fg-big", sprintf("%s · %s", round_title(s), phase)),
      shiny::p(shiny::strong(sprintf("%d of %d submitted", rs$n_confirmed, rs$n_expected)),
        sprintf("  ·  %d have a number entered.%s", rs$n_entered,
          if (length(rs$unconfirmed))
            paste0("  Not yet submitted: ", paste(rs$unconfirmed, collapse = ", "))
          else "  Everyone's in.")),
      timer_row,
      html_table(ad_pool_table(st)),
      html_table(ad_player_table(st)),
      shiny::hr(),
      controls,
      shiny::span(style = "margin-left:1rem;", shiny::downloadButton(ns("dl"), "Export CSV")),
      shiny::hr(),
      shiny::actionButton(ns("reset"), "Reset game to the lobby…", class = "btn btn-link text-danger")))
}

ad_proxy <- function(ns, st) {
  s <- st$session
  pl <- st$players[!is.na(st$players$pool_id), , drop = FALSE]
  alive <- st$pools$pool_id[is.na(st$pools$collapsed_round) | st$pools$collapsed_round >= s$current_round]
  pl <- pl[pl$pool_id %in% alive, , drop = FALSE]
  pl <- pl[order(pl$display_name), , drop = FALSE]
  if (nrow(pl) == 0) return(NULL)
  choices <- c("Pick a player…" = "", stats::setNames(pl$player_id, pl$display_name))
  shiny::div(class = "card",
    shiny::div(class = "card-header", "Enter an ask for a player whose phone isn't cooperating"),
    shiny::div(class = "card-body",
      shiny::fluidRow(
        shiny::column(5, shiny::selectInput(ns("proxy_player"), "Player", choices = choices,
                                            selectize = FALSE)),
        shiny::column(3, shiny::numericInput(ns("proxy_harvest"),
          sprintf("Ask (0 to %d)", s$max_harvest), value = NULL, min = 0, max = s$max_harvest, step = 1)),
        shiny::column(4, shiny::div(style = "margin-top:1.9rem;",
          shiny::actionButton(ns("proxy_submit"), "Enter for them", class = "btn btn-outline-secondary")))),
      shiny::p(class = "fg-muted", style = "font-size:.9rem;margin-bottom:0;",
        "Counts as their submission for this round; they can still change it from their own phone.")))
}

ad_leaderboard_table <- function(st) {
  lb <- fl_leaderboard(st)
  if (nrow(lb) == 0) return(NULL)
  data.frame(Rank = seq_len(nrow(lb)), Player = lb$display_name, Pool = pool_label(lb$pool_id),
             `Total catch` = fmt_fish(lb$total_catch),
             Prize = ifelse(lb$prize, "✓", ""), check.names = FALSE, stringsAsFactors = FALSE)
}

ad_pool_summary <- function(st) {
  pools <- st$pools
  if (nrow(pools) == 0) return(NULL)
  rows <- lapply(seq_len(nrow(pools)), function(i) {
    p <- pools[i, ]
    pr <- st$pool_rounds[st$pool_rounds$pool_id == p$pool_id, , drop = FALSE]
    data.frame(Pool = pool_label(p$pool_id), Fishers = p$n_players,
               `Rounds fished` = nrow(pr), `Total caught` = fmt_fish(sum(pr$harvested)),
               `Fish left` = if (is.na(p$collapsed_round)) fmt_fish(p$stock) else "0 (collapsed)",
               Status = pool_status(p), check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

ad_finished <- function(ns, st) {
  shiny::div(class = "card",
    shiny::div(class = "card-header", sprintf("Session #%d · finished", st$session$session_id)),
    shiny::div(class = "card-body",
      shiny::h5("Pools"), html_table(ad_pool_summary(st)),
      shiny::h5(sprintf("Players (prize at %s fish)", fmt_fish(st$session$prize_threshold))),
      html_table(ad_leaderboard_table(st)),
      shiny::downloadButton(ns("dl"), "Export CSV"), " ",
      shiny::actionButton(ns("newsession"), "New session", class = "btn btn-primary"),
      shiny::p(class = "fg-muted", style = "margin-top:.75rem;",
        "This session stays in History. The projector shows the chart and the leaderboard.")))
}
