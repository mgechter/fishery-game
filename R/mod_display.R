# Projector module: read-only big-screen view for the room.
#   URL:  <host>/?mode=display
# The chart redraws only when a round is scored; the submission count and the
# countdown stay live.
# (Structure adapted from Ari Anisfeld's pricing_game, MIT licence.)

display_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "fg-wide", style = "max-width:1500px;",
    shiny::tags$style(shiny::HTML(
      ".fg-disp h1{font-size:2.8rem;margin:.2rem 0} .fg-disp h2{font-size:1.9rem;color:#495057;margin:.3rem 0}
       .fg-disp table{font-size:1.35rem}
       .fg-pool{display:inline-block;min-width:260px;margin:.4rem 1.2rem .4rem 0;padding:.6rem 1rem;border:2px solid #dee2e6;border-radius:.5rem}
       .fg-pool .n{font-size:2.6rem;font-weight:700;line-height:1.1}
       .fg-pool .l{font-size:1.2rem;color:#6c757d}
       .fg-pool.dead{border-color:#dc3545;color:#dc3545}")),
    shiny::div(class = "fg-disp", shiny::uiOutput(ns("screen"))))
}

display_server <- function(id, con) shiny::moduleServer(id, function(input, output, session) {
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

  # Redraw the chart only when the scored picture changes.
  chart_key <- shiny::reactiveVal("")
  shiny::observe({
    st <- state()
    k <- if (is.null(st)) "none" else
      paste(st$session$session_id, st$session$phase, st$session$current_round, nrow(st$pool_rounds))
    if (!identical(k, shiny::isolate(chart_key()))) chart_key(k)
  })
  chart_state <- shiny::reactive({ chart_key(); shiny::isolate(state()) })

  output$timer <- shiny::renderUI({
    shiny::invalidateLater(1000)
    st <- state(); if (is.null(st)) return(NULL)
    countdown_tag(fl_time_left(st$session), size = "5rem")
  })
  output$chart <- shiny::renderPlot({
    st <- shiny::req(chart_state())
    shiny::req(nrow(st$pools) > 0)
    plot_stock(st, base_size = 20)
  }, height = 460)
  output$lb <- shiny::renderTable({
    st <- shiny::req(chart_state())
    ad_leaderboard_table(st)
  }, striped = TRUE, width = "100%")

  output$screen <- shiny::renderUI({
    st <- state()
    if (is.null(st)) return(shiny::h1("The fishery game: waiting for the instructor to start…"))
    s <- st$session; rs <- st$round_status
    pool_cards <- if (nrow(st$pools)) lapply(seq_len(nrow(st$pools)), function(i) {
      p <- st$pools[i, ]
      dead <- !is.na(p$collapsed_round)
      shiny::div(class = paste("fg-pool", if (dead) "dead"),
        shiny::div(class = "l", sprintf("%s · %d fishers", pool_label(p$pool_id), p$n_players)),
        shiny::div(class = "n", if (dead) "collapsed" else fmt_fish(p$stock)),
        shiny::div(class = "l", if (dead) sprintf("in round %d", p$collapsed_round)
                   else sprintf("fish now (max %s)", fmt_fish(p$start_stock))))
    })
    shiny::tagList(
      shiny::div(style = "display:flex;justify-content:space-between;align-items:center;gap:2rem;",
        shiny::h1("The fishery game"),
        shiny::uiOutput(ns("timer"))),
      switch(s$phase,
        lobby    = shiny::h2(sprintf("Lobby: %d player(s) have joined", nrow(st$players))),
        finished = shiny::h2("Game over"),
        shiny::h2(sprintf("%s · %d of %d submitted", round_title(s), rs$n_confirmed, rs$n_expected))),
      shiny::div(pool_cards),
      if (nrow(st$pools)) shiny::plotOutput(ns("chart"), height = "460px"),
      if (s$phase == "finished") shiny::tagList(
        shiny::h2(sprintf("Total catch (prize at %s)", fmt_fish(s$prize_threshold))),
        shiny::tableOutput(ns("lb"))))
  })
})
