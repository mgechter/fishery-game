# Small shared UI helpers (plain Shiny tags).
# (Adapted from Ari Anisfeld's pricing_game, MIT licence.)

# Whole fish print as integers, prorated shares with one decimal.
fmt_fish <- function(x) {
  x <- as.numeric(x)
  vapply(x, function(v) {
    if (is.na(v)) return("—")
    if (abs(v - round(v)) < 1e-6) formatC(round(v), format = "d", big.mark = ",")
    else formatC(v, format = "f", digits = 1, big.mark = ",")
  }, "")
}

fmt_time <- function(epoch) format(as.POSIXct(as.numeric(epoch), origin = "1970-01-01"),
                                   "%H:%M:%S")

round_title <- function(s) {
  if (isTRUE((s$show_rounds %||% 1) == 1)) sprintf("Round %d of %d", s$current_round, s$n_rounds)
  else sprintf("Round %d", s$current_round)
}

# Render a data.frame as a Bootstrap table inside a renderUI() block.
html_table <- function(df, class = "table table-sm") {
  if (is.null(df) || nrow(df) == 0) return(shiny::p(class = "fg-muted", "—"))
  cells <- function(r) lapply(df[r, ], function(v) shiny::tags$td(as.character(v)))
  shiny::tags$table(class = class,
    shiny::tags$thead(shiny::tags$tr(lapply(names(df), shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(df)), function(r) shiny::tags$tr(cells(r)))))
}

# One labelled statistic for a result card.
stat_block <- function(label, value) shiny::div(
  style = "display:inline-block; min-width:120px; margin:.3rem 1.2rem .3rem 0;",
  shiny::div(class = "fg-muted", style = "font-size:.85rem;", label),
  shiny::div(style = "font-size:1.5rem; font-weight:600;", value)
)

# A styled countdown. `tl` = seconds left (NA -> nothing).
countdown_tag <- function(tl, size = "2.2rem", note = NULL) {
  if (length(tl) != 1 || is.na(tl)) return(NULL)
  col <- if (tl <= 0) "#dc3545" else if (tl <= 15) "#fd7e14" else "#198754"
  txt <- if (tl <= 0) "0:00" else sprintf("%d:%02d", tl %/% 60, floor(tl) %% 60)
  shiny::div(
    style = sprintf("font-weight:700;font-variant-numeric:tabular-nums;line-height:1;color:%s;font-size:%s;", col, size),
    txt,
    if (!is.null(note)) shiny::div(style = "font-size:.85rem;font-weight:500;color:#6c757d;", note))
}

base_url <- function(session) {
  cd   <- session$clientData
  port <- if (!is.null(cd$url_port) && nzchar(cd$url_port)) paste0(":", cd$url_port) else ""
  paste0(cd$url_protocol, "//", cd$url_hostname, port, cd$url_pathname)
}

# Status of a pool for a table cell.
pool_status <- function(pool) {
  if (is.na(pool$collapsed_round)) "alive" else sprintf("collapsed in round %d", pool$collapsed_round)
}
