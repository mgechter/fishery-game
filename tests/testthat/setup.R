# Source the app's R files once for the whole suite. test_dir runs with the
# working directory at tests/testthat, so climb to the project root.
root <- normalizePath(file.path(getwd(), "..", ".."))
for (f in c("engine.R", "db.R", "game_logic.R", "ui_helpers.R", "plots.R"))
  source(file.path(root, "R", f))

fresh_db <- function() {
  path <- tempfile("fishery_test_", fileext = ".sqlite")
  con <- db_connect(path)
  db_init(con)
  con
}
