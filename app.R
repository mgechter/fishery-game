# The fishery game: a Shiny app for Hazlett's common-pool resource experiment.
#
#   Player view:      <url>/
#   Instructor view:  <url>/?role=admin&key=<FISHERY_ADMIN_KEY>
#   Projector view:   <url>/?mode=display
#
# Env vars (all optional):
#   FISHERY_DB         sqlite file path   (default data/fishery.sqlite, falling
#                                          back to the temp folder if that is not writable)
#   FISHERY_ADMIN_KEY  instructor key     (default "hazlett")
#
# Structure adapted from Ari Anisfeld's pricing_game (MIT licence).

library(shiny)
library(bslib)

source("R/engine.R")
source("R/db.R")
source("R/game_logic.R")
source("R/ui_helpers.R")
source("R/plots.R")
source("R/mod_player.R")
source("R/mod_admin.R")
source("R/mod_display.R")

ADMIN_KEY <- Sys.getenv("FISHERY_ADMIN_KEY", "hazlett")
DB_PATH   <- Sys.getenv("FISHERY_DB", "data/fishery.sqlite")

open_db <- function(path) {
  writable <- tryCatch({
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    dir.exists(dirname(path)) && file.access(dirname(path), 2) == 0
  }, error = function(e) FALSE)
  if (!writable) {
    path <- file.path(tempdir(), basename(path))
    message("app folder not writable; database in ", path)
  }
  con <- db_connect(path)
  db_init(con)
  message("database: ", normalizePath(path))
  con
}
.con <- open_db(DB_PATH)
onStop(function() try({ db_checkpoint(.con); DBI::dbDisconnect(.con) }, silent = TRUE))

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#9D2235"),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("The fishery game"),
    tags$style(HTML("
    .fg-narrow { max-width: 560px;  margin: 1rem auto; }
    .fg-wide   { max-width: 1200px; margin: 1rem auto; }
    .fg-big    { font-size: 1.8rem; font-weight: 600; margin: .4rem 0 .6rem; }
    .fg-muted  { color: #6c757d; }
    .card      { margin-bottom: 1rem; }
    .btn-lg    { font-size: 1.25rem; padding: .6rem 1.4rem; }
    input[type=number] { font-size: 1.6rem; }
  "))),
  uiOutput("root")
)

server <- function(input, output, session) {
  q    <- parseQueryString(isolate(session$clientData$url_search))
  mode <- if (identical(q$mode, "display")) "display"
          else if (identical(q$role, "admin")) "admin"
          else "player"
  ok_key <- identical(q$key, ADMIN_KEY)

  output$root <- renderUI({
    switch(mode,
      display = display_ui("display"),
      admin   = if (ok_key) admin_ui("admin")
                else div(class = "fg-narrow", div(class = "card", div(class = "card-body",
                  h4("Instructor key required"),
                  p(class = "fg-muted", "Append ", tags$code("&key=…"), " to the URL.")))),
      player  = player_ui("player"))
  })

  switch(mode,
    display = display_server("display", .con),
    admin   = if (ok_key) admin_server("admin", .con),
    player  = player_server("player", .con))
}

shinyApp(ui, server)
