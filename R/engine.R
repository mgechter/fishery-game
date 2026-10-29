# Pure game engine for the fishery (common-pool resource) game. No Shiny, no
# database, so every rule is testable on its own (tests/testthat/test-engine.R).
#
# The rules are Denise Hazlett's classroom experiment ("A Common Property
# Experiment with a Renewable Resource", Economic Inquiry 35, 1997), as
# economics-games.com ran it until September 2026:
#   - a pool starts with fish_per_player x (players in the pool) fish, and that
#     number is also the most the pool can ever hold
#   - each round, each player asks for 0 to max_harvest fish
#   - if the asks add up to no more than the stock, everyone gets their ask;
#     otherwise each player gets a prorated share of the stock
#   - the fish left after the harvest reproduce (each has one offspring, so the
#     stock doubles, up to the cap) provided a viable population remains, which
#     is one fish per player in the pool; below that the pool collapses and
#     that pool's game is over

`%||%` <- function(a, b) if (is.null(a)) b else a

# One round for one pool.
#   stock   fish in the pool at the start of the round
#   asks    numeric vector, one entry per player (0 .. max_harvest)
#   cap     the pool's maximum (its starting stock)
#   viable  the viable population (players in the pool, one fish each)
#   growth  multiplier applied to the survivors (2 = each fish has one offspring)
# Returns the catches (same order as asks) and the pool's bookkeeping.
resolve_pool_round <- function(stock, asks, cap, viable, growth = 2) {
  asks  <- as.numeric(asks)
  stock <- as.numeric(stock)
  if (length(asks) == 0) stop("a pool needs at least one player", call. = FALSE)
  if (any(is.na(asks)) || any(asks < 0)) stop("asks must be numbers of 0 or more", call. = FALSE)
  total <- sum(asks)
  catches <- if (total <= stock || total == 0) asks else asks * (stock / total)
  harvested <- sum(catches)
  remaining <- round(max(0, stock - harvested), 6)
  collapsed <- remaining < viable
  stock_next <- if (collapsed) 0 else min(cap, remaining * growth)
  list(
    catches    = catches,
    total_ask  = total,
    harvested  = harvested,
    remaining  = remaining,
    collapsed  = collapsed,
    stock_next = stock_next
  )
}

# Deal players into pools: shuffle, then round-robin, so the pools differ in
# size by at most one. Returns a data.frame(player_id, pool_id).
assign_pools <- function(player_ids, n_pools, seed = NULL) {
  n <- length(player_ids)
  if (n < 1) stop("need at least one player", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  n_pools <- max(1L, min(as.integer(n_pools), n))
  ids <- player_ids[sample.int(n)]           # sample.int, never sample(x): a
                                             # length-one numeric x would be read as 1:x
  data.frame(player_id = ids,
             pool_id   = ((seq_len(n) - 1L) %% n_pools) + 1L,
             stringsAsFactors = FALSE)
}

pool_label <- function(pool_id) {
  pool_id <- as.integer(pool_id)
  ifelse(is.na(pool_id), "—", paste("Pool", LETTERS[pmin(pool_id, 26L)]))
}
