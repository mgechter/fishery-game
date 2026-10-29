# The one chart: fish in each pool at the start of every round.

# Rows: one per pool per round boundary. Round 0 is the starting stock; round r
# is the stock after round r's reproduction (what round r + 1 opens with).
stock_path <- function(st) {
  pools <- st$pools
  if (is.null(pools) || nrow(pools) == 0) return(NULL)
  parts <- lapply(seq_len(nrow(pools)), function(i) {
    p <- pools[i, ]
    pr <- st$pool_rounds[st$pool_rounds$pool_id == p$pool_id, , drop = FALSE]
    pr <- pr[order(pr$round), , drop = FALSE]
    data.frame(pool = pool_label(p$pool_id),
               round = c(0L, pr$round),
               stock = c(p$start_stock, pr$stock_next),
               collapsed = c(FALSE, pr$collapsed == 1),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, parts)
}

plot_stock <- function(st, base_size = 18) {
  d <- stock_path(st)
  if (is.null(d)) return(NULL)
  n_rounds <- st$session$n_rounds
  cap <- max(st$pools$start_stock)
  g <- ggplot2::ggplot(d, ggplot2::aes(x = round, y = stock, colour = pool, group = pool)) +
    ggplot2::geom_line(linewidth = 1.4) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::geom_point(data = d[d$collapsed, , drop = FALSE], shape = 4, size = 7, stroke = 2) +
    ggplot2::scale_x_continuous(breaks = 0:n_rounds, limits = c(0, n_rounds),
                                labels = function(x) ifelse(x == 0, "start", x)) +
    ggplot2::scale_y_continuous(limits = c(-cap * 0.04, cap * 1.05), expand = c(0, 0),
                                breaks = function(l) pretty(c(0, l[2]))) +
    ggplot2::labs(x = "after round", y = "fish in the pool", colour = NULL) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = "top",
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
  if (any(d$collapsed))
    g <- g + ggplot2::annotate("text", x = n_rounds, y = cap * 0.02, hjust = 1, vjust = 0,
                               label = "× = collapsed", size = base_size / 3.5, colour = "#495057")
  g
}
