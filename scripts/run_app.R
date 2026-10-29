#!/usr/bin/env Rscript
# Launch the fishery game locally. From the project root:
#
#   Rscript scripts/run_app.R                         # port 3838
#   FISHERY_ADMIN_KEY=secret Rscript scripts/run_app.R
#
# Player link:      http://<host>:3838/
# Instructor link:  http://<host>:3838/?role=admin&key=<FISHERY_ADMIN_KEY>
# Projector link:   http://<host>:3838/?mode=display

port <- as.integer(Sys.getenv("FISHERY_PORT", "3838"))
key  <- Sys.getenv("FISHERY_ADMIN_KEY", "hazlett")

cat(sprintf("\n  Database:   %s\n", Sys.getenv("FISHERY_DB", "data/fishery.sqlite")))
cat(sprintf("  Player:     http://127.0.0.1:%d/\n", port))
cat(sprintf("  Instructor: http://127.0.0.1:%d/?role=admin&key=%s\n", port, key))
cat(sprintf("  Projector:  http://127.0.0.1:%d/?mode=display\n\n", port))

shiny::runApp("app.R", host = "0.0.0.0", port = port, launch.browser = FALSE)
