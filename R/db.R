# SQLite persistence for the fishery game.
#
# One file. Shiny serialises every session on a single R thread, so inside one
# worker there is no write concurrency; WAL mode plus a busy timeout cover a
# second worker process or the play-test bot writing the same file.
# Game logic lives in R/game_logic.R; this file is only the store.
# (Adapted from Ari Anisfeld's pricing_game, MIT licence.)

DB_SCHEMA <- "
CREATE TABLE IF NOT EXISTS meta (
  key   TEXT PRIMARY KEY,
  value TEXT
);
CREATE TABLE IF NOT EXISTS sessions (
  session_id       INTEGER PRIMARY KEY AUTOINCREMENT,
  label            TEXT,
  phase            TEXT    NOT NULL DEFAULT 'lobby',   -- lobby | open | scored | finished
  current_round    INTEGER NOT NULL DEFAULT 0,
  n_rounds         INTEGER NOT NULL,
  n_pools          INTEGER NOT NULL,
  max_harvest      INTEGER NOT NULL,
  fish_per_player  INTEGER NOT NULL,
  growth           REAL    NOT NULL DEFAULT 2,
  show_rounds      INTEGER NOT NULL DEFAULT 1,         -- players see 'Round r of N' (1) or 'Round r' (0)
  prize_threshold  REAL    NOT NULL DEFAULT 45,
  round_started_at REAL,
  round_seconds    INTEGER NOT NULL DEFAULT 0,         -- countdown length, 0 = no timer
  auto_advance     INTEGER NOT NULL DEFAULT 0,         -- also open the next round after an auto-score
  carry_forward    INTEGER NOT NULL DEFAULT 0,         -- a no-show reuses last round's ask (1) or asks 0 (0)
  created_at       REAL    NOT NULL
);
CREATE TABLE IF NOT EXISTS players (
  player_id    INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id   INTEGER NOT NULL,
  display_name TEXT    NOT NULL,
  token        TEXT    NOT NULL UNIQUE,
  pool_id      INTEGER,                                -- NULL until the game starts
  joined_at    REAL    NOT NULL
);
CREATE TABLE IF NOT EXISTS pools (
  session_id      INTEGER NOT NULL,
  pool_id         INTEGER NOT NULL,
  n_players       INTEGER NOT NULL,
  start_stock     REAL    NOT NULL,                    -- also the cap
  stock           REAL    NOT NULL,                    -- fish at the start of the round about to be played
  collapsed_round INTEGER,                             -- NULL while the pool is alive
  PRIMARY KEY (session_id, pool_id)
);
CREATE TABLE IF NOT EXISTS submissions (
  session_id   INTEGER NOT NULL,
  round        INTEGER NOT NULL,
  player_id    INTEGER NOT NULL,
  harvest      REAL    NOT NULL,
  confirmed    INTEGER NOT NULL DEFAULT 0,             -- 1 = the player pressed Submit
  carried      INTEGER NOT NULL DEFAULT 0,             -- 1 = carried forward at scoring
  submitted_at REAL    NOT NULL,
  PRIMARY KEY (session_id, round, player_id)
);
CREATE TABLE IF NOT EXISTS results (
  session_id       INTEGER NOT NULL,
  round            INTEGER NOT NULL,
  player_id        INTEGER NOT NULL,
  pool_id          INTEGER NOT NULL,
  ask              REAL    NOT NULL,
  catch            REAL    NOT NULL,
  cumulative_catch REAL    NOT NULL,
  no_submission    INTEGER NOT NULL DEFAULT 0,
  computed_at      REAL    NOT NULL,
  PRIMARY KEY (session_id, round, player_id)
);
CREATE TABLE IF NOT EXISTS pool_rounds (
  session_id  INTEGER NOT NULL,
  round       INTEGER NOT NULL,
  pool_id     INTEGER NOT NULL,
  stock_start REAL    NOT NULL,
  total_ask   REAL    NOT NULL,
  harvested   REAL    NOT NULL,
  remaining   REAL    NOT NULL,
  stock_next  REAL    NOT NULL,
  collapsed   INTEGER NOT NULL DEFAULT 0,
  computed_at REAL    NOT NULL,
  PRIMARY KEY (session_id, round, pool_id)
);
"

db_connect <- function(path) {
  if (is.null(path) || !nzchar(path))
    stop("empty database path (FISHERY_DB set but blank?)", call. = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")
  DBI::dbExecute(con, "PRAGMA synchronous = NORMAL;")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 15000;")
  DBI::dbExecute(con, "PRAGMA wal_autocheckpoint = 200;")
  con
}

# Columns added after the first release, applied to a database created before them.
DB_MIGRATIONS <- c(
  "ALTER TABLE sessions ADD COLUMN carry_forward INTEGER NOT NULL DEFAULT 0"
)

db_init <- function(con) {
  for (stmt in strsplit(DB_SCHEMA, ";\\s*\n")[[1]]) {
    stmt <- trimws(stmt)
    if (nzchar(stmt)) DBI::dbExecute(con, stmt)
  }
  for (stmt in DB_MIGRATIONS) {
    tryCatch(DBI::dbExecute(con, stmt), error = function(e) NULL)  # already applied
  }
  db_checkpoint(con)
  invisible(TRUE)
}

db_checkpoint <- function(con) {
  tryCatch(DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE);"),
           error = function(e) NULL)
}

# --- thin query helpers ---------------------------------------------------

db_run <- function(con, sql, params = list()) {
  if (length(params)) DBI::dbExecute(con, sql, params = params)
  else                DBI::dbExecute(con, sql)
}

db_query <- function(con, sql, params = list()) {
  if (length(params)) DBI::dbGetQuery(con, sql, params = params)
  else                DBI::dbGetQuery(con, sql)
}

db_one <- function(con, sql, params = list()) {
  d <- db_query(con, sql, params)
  if (nrow(d) == 0) NULL else as.list(d[1, , drop = FALSE])
}

db_last_id <- function(con) {
  db_query(con, "SELECT last_insert_rowid() AS id")$id
}

meta_get <- function(con, key) {
  v <- db_one(con, "SELECT value FROM meta WHERE key = ?", list(key))
  if (is.null(v)) NULL else v$value
}

meta_set <- function(con, key, value) {
  db_run(con,
    "INSERT INTO meta (key, value) VALUES (?, ?)
     ON CONFLICT(key) DO UPDATE SET value = excluded.value",
    list(key, as.character(value)))
}

now_epoch <- function() as.numeric(Sys.time())

random_token <- function(n = 12) {
  paste(sample(c(letters, 0:9), n, replace = TRUE), collapse = "")
}
