# The fishery game

A Shiny app for running Denise Hazlett's common-pool resource experiment in
class ("A Common Property Experiment with a Renewable Resource", *Economic
Inquiry* 35, 1997), the game economics-games.com offered until it closed in
September 2026. Students ask for a harvest each round on their phones; the
instructor runs the rounds from an admin screen; a projector page shows each
pool's stock over the rounds and, at the end, the leaderboard.

The app's structure (one R process, one SQLite file, player, instructor and
projector views off one URL, one-second polling, a play-test bot) is adapted
from Ari Anisfeld's [pricing_game](https://github.com/anisfeld/pricing_game),
MIT licence. The game logic is new.

## The rules

- A pool starts with 10 fish per player in it, and can never hold more.
- Each round, every player asks for 0 to 20 fish. If the asks add up to no
  more than the stock, everyone gets their ask; otherwise each player gets a
  share of the stock in proportion to their ask.
- The fish left after the harvest double for the next round, up to the cap,
  provided at least one fish per player remains. Below that the pool collapses
  and that pool's game is over.
- The game runs a set number of rounds (11 by default) or until every pool has
  collapsed. By default the whole class fishes one pool; with more than one,
  players are dealt into the pools at random when the instructor presses
  Start. A player's payoff is their total catch; the admin screen marks who
  reached the prize threshold (45 by default).

All of these are settings on the instructor's "Start a new session" screen.

## Run it locally

```sh
Rscript scripts/run_app.R          # serves on port 3838
```

| view | URL |
|---|---|
| Student | `http://<host>:3838/` |
| Instructor | `http://<host>:3838/?role=admin&key=hazlett` |
| Projector | `http://<host>:3838/?mode=display` |

Set `FISHERY_ADMIN_KEY` to change the instructor key. Needs R with shiny,
bslib, DBI, RSQLite and ggplot2 (plus testthat for the tests).

## Run a class

1. Open the instructor URL, set the pools, rounds and prize threshold, press
   **Create session**.
2. Put the projector URL on the screen and give students the student URL.
   Each student enters a name and lands in the lobby. Their identity is kept
   in the URL (`?t=…`), so a reload resumes them.
3. Press **Start game**. Students are dealt into pools and round 1 opens.
4. Each round: students type an ask (it saves as they type) and press Submit.
   The admin screen shows who is in. Press **Close and score the round**, then
   **Next round**. Someone who entered nothing is scored as asking for 0; the
   carry-forward box reuses their previous ask instead, and a round can be
   re-scored before you move on. An optional round timer scores the round at
   0:00.
5. After the last round, or once every pool has collapsed, the leaderboard
   shows on the admin and projector pages; **Export CSV** gives one row per
   player per round with the pool's bookkeeping.

## Deploy on Posit Connect Cloud

Connect Cloud publishes straight from this GitHub repository: pick the repo,
the branch and `app.R` as the primary file; the `manifest.json` here lists the
packages. Three content settings matter for a game whose state is shared:

- **Max worker processes: 1** and **max connections per worker: 200**, so
  every student talks to the same R process.
- **Idle timeout: 60 s** (the maximum), and keep the projector or instructor
  tab open from before students join until the debrief, which keeps that
  process alive.
- Set `FISHERY_ADMIN_KEY` as a secret environment variable.

**A push redeploys the app, and a redeploy empties its database.** Connect
Cloud republishes on every push to the connected branch, and the SQLite file
lives in the container, which a new build replaces: on 2026-10-29 a push at
12:53 left History reading "No sessions yet" at 12:59. So push before creating
an evening's session or after its debrief, never between; a worker idle
within one build kept its state in an 80-second test. The free plan deploys
from public repositories only, which is why this one is public.

Regenerate the manifest after changing the packages the app uses:

```r
rsconnect::writeManifest(appPrimaryDoc = "app.R")
```

## Testing

```sh
Rscript scripts/run_tests.R                 # engine, game loop and module observers, headless
Rscript scripts/run_app.R                   # terminal 1
Rscript scripts/playtest_bot.R              # terminal 2: ten bots join and play
```

Start the bot before pressing Start in the instructor view. Keep a play-test
off real data by giving both the same scratch file, `FISHERY_DB=data/playtest.sqlite`.

`tests/testthat/test-adversarial.R` is an adversarial audit written by OpenAI
Codex on 2026-10-29 (its report: `docs/codex-adversarial-report-2026-10-29.md`).
Ten of its eleven defect blocks failed on the first version and pass since the
fixes of the same day: a stale instructor tab can no longer score or advance a
round it has not seen, the reset dialog resets only the session it was opened
for, the timer reads the live session row rather than the poll's copy, Start is
one transaction, a draft typed in one round cannot land in the next, the no-show
policy is stored with the session, and the state stamp and the projector's chart
key notice a re-score, a reset and restart, and two writes within a millisecond.
The eleventh, the countdown scoring with only the projector connected, is a
design choice and is skipped: the instructor tab scores the timer.
