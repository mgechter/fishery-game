#!/usr/bin/env Rscript
# Drive the players during a live play-test. Start the app in one terminal,
# this in another (BEFORE pressing Start in the instructor view), then run the
# round loop from the instructor view: the bots join the lobby, and each open
# round they enter an ask after a short random delay. They never advance the
# game themselves.
#
#   Rscript scripts/playtest_bot.R
#   PLAYTEST_NAMES="Ada,Boaz,Cleo" Rscript scripts/playtest_bot.R
#   PLAYTEST_MINDELAY=1 PLAYTEST_MAXDELAY=4 Rscript scripts/playtest_bot.R
#
# Each bot has a stable style, from a hash of its name: greedy (asks the max),
# steady (asks about a tenth of the pool), or cautious (asks 2 to 5).
# Keep the play-test off real data:  FISHERY_DB=data/playtest.sqlite  for both.

source("R/engine.R"); source("R/db.R"); source("R/game_logic.R")

db_path <- Sys.getenv("FISHERY_DB", "data/fishery.sqlite")
names   <- strsplit(Sys.getenv("PLAYTEST_NAMES",
  "Ada,Boaz,Cleo,Dmitri,Esme,Farid,Gwen,Hugo,Iris,Jun"), ",")[[1]]
names   <- trimws(names)
mind    <- as.numeric(Sys.getenv("PLAYTEST_MINDELAY", "1"))
maxd    <- as.numeric(Sys.getenv("PLAYTEST_MAXDELAY", "6"))

con <- db_connect(db_path)
cat("bot: database", normalizePath(db_path), "\n")

style_of <- function(name) {
  h <- sum(utf8ToInt(name)) %% 3
  c("greedy", "steady", "cautious")[h + 1]
}
decide <- function(style, max_harvest, stock, n_players) {
  switch(style,
    greedy   = max_harvest,
    steady   = max(0, min(max_harvest, round(stock / n_players / 2))),
    cautious = sample(2:5, 1))
}

joined <- list()          # session_id -> named list of player_id by name
due    <- list()          # "sid:round:name" -> time to submit
done   <- character(0)

repeat {
  s <- fl_active_session(con)
  if (!is.null(s)) {
    sid <- as.character(s$session_id)
    if (s$phase == "lobby") {
      existing <- fl_players(con, s$session_id)
      for (nm in names) {
        if (nm %in% existing$display_name) next
        r <- tryCatch(fl_add_player(con, s$session_id, nm), error = function(e) NULL)
        if (!is.null(r)) cat(sprintf("bot: %s joined session %s\n", nm, sid))
      }
    } else if (s$phase == "open") {
      pl <- fl_players(con, s$session_id)
      pools <- fl_pools(con, s$session_id)
      for (nm in names) {
        key <- paste(sid, s$current_round, nm, sep = ":")
        if (key %in% done) next
        me <- pl[pl$display_name == nm, , drop = FALSE]
        if (nrow(me) == 0 || is.na(me$pool_id[1])) next
        pool <- pools[pools$pool_id == me$pool_id[1], , drop = FALSE]
        if (nrow(pool) == 0 || (!is.na(pool$collapsed_round[1]) && pool$collapsed_round[1] < s$current_round)) {
          done <- c(done, key); next
        }
        if (is.null(due[[key]])) { due[[key]] <- Sys.time() + stats::runif(1, mind, maxd); next }
        if (Sys.time() < due[[key]]) next
        ask <- decide(style_of(nm), s$max_harvest, pool$stock[1], pool$n_players[1])
        ok <- tryCatch({ fl_submit(con, s$session_id, me$player_id[1], s$current_round, ask, confirm = TRUE); TRUE },
                       error = function(e) { cat("bot:", nm, conditionMessage(e), "\n"); FALSE })
        if (ok) cat(sprintf("bot: round %d, %s (%s) asks %d\n", s$current_round, nm, style_of(nm), ask))
        done <- c(done, key)
      }
    }
  }
  Sys.sleep(0.5)
}
