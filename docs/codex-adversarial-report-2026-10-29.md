Adversarial audit, 2026-10-29. **The default rule arithmetic held; 11 state/UI defects were demonstrated.**
Only tests and this report were added. No application changes, installations, commits, or pushes.
Read README first, then all requested implementation files. Tests use SQLite, the actual Shiny module observers via `testServer`, controlled clocks, and explicit interruption injection; no browser automation.
Every D-number below names a failing block in [test-adversarial.R](tests/testthat/test-adversarial.R).

**Findings, ranked by severity**

1. **High — D10: Reset can delete a different session's rounds.**
   Reproduction: open Reset for session A; another instructor activates B; allow one poll; confirm the still-open dialog.
   Actual: B returns to the lobby and its saved ask of 7 disappears. A remains open.
   Location: `R/mod_admin.R:61–74`; the confirmation resolves `sid()` again.
   Fix: capture session ID and game generation when opening the modal; reject a stale confirmation or reset only that captured target.

2. **High — D9: timer/manual-score race skips an entire round.**
   Reproduction: let the poll cache expired round 1; manually score and advance; then run the timer observer.
   Actual: the timer scores empty round 2 and auto-advances to round 3. The test models a 50 ms startup-history delay to separate the independent observer deadlines.
   Location: `R/mod_admin.R:172–188`; expiry is checked against cached state, while scoring acts on the database's current round.
   Fix: read the current database row at timer execution; atomically verify session/generation/round, open phase, and deadline before scoring and advancing that same round.

3. **High — D2: a stale instructor Close scores the next round.**
   Reproduction: tab A still displays round 1; tab B scores it and opens round 2; A presses Close before polling.
   Actual: round 2 is scored with zeros and two extra result rows appear.
   Location: `R/mod_admin.R:46–59`, `R/game_logic.R:288`.
   Fix: bind the button action to the displayed session/generation/round and require that exact identity in `fl_score_round`; preserve deliberate same-round re-scoring.

4. **High — D7: a pending draft is attributed to the next round.**
   Reproduction: type 8 at mock time 600 ms; close/advance before the 900 ms debounce fires; poll at 1000 ms; deliver the draft at 1500 ms.
   Actual: round 2 receives an unconfirmed ask of 8 without a round-2 entry. The test models a slow phone that has not yet returned the new input's blank value.
   Location: `R/mod_player.R:86–95`; only the number is debounced, then the current round is fetched.
   Fix: capture the value together with its round/generation before debouncing, cancel obsolete drafts, and include a round identifier in client submission events.

5. **Medium — D6: interrupted Start leaves a lobby that cannot start.**
   Reproduction: inject worker loss immediately after the first pool INSERT commits; disconnect, reopen the same SQLite file, and press Start again.
   Actual: `UNIQUE constraint failed: pools.session_id, pools.pool_id`; partially assigned players/pools remain while phase is `lobby`.
   Location: `R/game_logic.R:125–147`.
   Fix: wrap assignments, all pool inserts, and the phase transition in one `DBI::dbWithTransaction`; provide recovery for existing partial lobbies.

6. **Medium — D8: reset/restart can be invisible to every poller.**
   Reproduction: with no submissions/results, reset and start between polls, changing the pool seed and start time.
   Actual: player assignments and the timer deadline change, but `fl_state_stamp` is identical. Cached screens can keep old pools/deadlines until another stamped change occurs.
   Location: `R/game_logic.R:360–373`; neither game generation nor `round_started_at` participates.
   Fix: persist a generation and a monotonic state revision, incremented transactionally on reset/start and all visible mutations; use these in screen and poll keys.

7. **Medium — D4: carry-forward policy is lost on instructor reconnection.**
   Reproduction: ask 4 in round 1; open round 2; enable carry-forward; close and recreate the instructor module; score a no-show.
   Actual: the ask is 0 instead of 4. This also permits different instructor tabs to use different policies.
   Location: `R/mod_admin.R:23–24`; `carry_rv` exists only in that R session.
   Fix: store the policy in `sessions`, persist checkbox changes, and read it for both manual and timer scoring.

8. **Medium — D3: a reset can disable the timer for round 1.**
   Reproduction: auto-score round 1; reset to lobby; start again in the same instructor tab; let the new deadline expire.
   Actual: the round stays open because `last_auto()` is still 1.
   Location: `R/mod_admin.R:172–178`.
   Fix: key automatic completion by session/generation/round, or clear the latch on generation changes; enforce idempotency in the database.

9. **Medium — D11: a projector alone does not keep automatic scoring running.**
   Reproduction: start a timed round, keep only the projector module connected, advance past the persisted deadline and poll twice.
   Actual: phase remains `open`; the countdown can reach zero without scoring. Reconnecting an instructor does recover the deadline (passing guard).
   Location: timer execution exists only inside `admin_server` in `R/mod_admin.R:172–191`.
   Fix: run one worker-level scheduler using persisted deadlines and atomic round guards, independent of instructor connections.

10. **Medium — D1: projector chart contradicts re-scored results.**
    Reproduction: one player asks 8 in round 1; round 2 is scored as a no-show, then re-scored with carry-forward.
    Actual: database/current projector state changes from 8 fish to collapsed/0, but the chart's cached state stays at 8.
    Location: `R/mod_display.R:33–41`; re-scoring changes neither phase, round, nor row count.
    Fix: invalidate chart state on a persisted score revision or changed pool-round values, rather than row count alone.

11. **Low — D5: sub-millisecond changes collide in the state stamp.**
    Reproduction: update an existing ask from 4 to confirmed 9 at timestamps 1000 and 1000.0001.
    Actual: saved value/confirmation changes but the stamp does not; millisecond formatting rounds both timestamps identically.
    Location: `R/game_logic.R:371–373`.
    Fix: use the transactional integer revision proposed for D8; wall-clock maxima and rounded timestamps are not reliable change counters.

**Attacks that held up**

- All 441 two-player ask combinations, all 21 single-player asks, and 500 seeded proration cases: stock conservation within floating-point tolerance, cap, exact-stock collapse, inclusive viability, and removal of tiny proration remainders.
- Twenty-five re-scores preserve fractional catches and cumulative totals; last-round collapse and all-pools-collapse termination work through the normal Finish/Next transition.
- Missing entries score zero; carry-forward in round 1 uses zero; carry-forward after a prior no-show does not resurrect an older ask.
- In the stated single-threaded worker model, duplicate-name joins serialize and the second is rejected. Late joins, duplicate Start/Next, stale round submissions, submissions after Close, and collapsed-pool proxy entries are rejected. Double Close within the same round is idempotent.
- Reset during an open round preserves roster/tokens and clears game data. Deleted tokens lose identity; URL tokens restore players and survive reset.
- Negative, decimal, huge, infinite, missing, vector, and nonnumeric asks cannot overwrite a saved ask. Whole-valued scientific notation is accepted. Actual numeric-input observers also reject invalid values; confirmation remains sticky.
- Empty/200-character names are rejected. HTML names and session labels are escaped in rendered lobby/admin/proxy/history/projector-table output; Markdown remains literal. Exact admin-key value comparison rejects missing, wrong-case, partial, padded, and malformed wrong-key inputs.
- Lobby/open/scored/finished data and all three module screens rebuild after closing/reopening SQLite. Timer deadline/auto-advance recover on instructor reconnection. Retrying an interrupted score uses persisted starting stocks without double counting. Carry-forward policy is the D4 exception.

**Polling measurement**

Instrumented `db_query` and an actual player module: each idle poll performs **6 SELECTs**, so **150/s for 25 clients**, or **162/s for 25 students + instructor + projector**.
A changed poll additionally performs 11 state-loading SELECTs, plus one token lookup per player: **484 SELECTs in a tick where all 27 reload** (162 + 27×11 + 25), excluding mutation/history queries and initial setup.
An actual player's changed tick measured 18 reads versus 6 idle. This is a query count, not a 25-phone latency/load guarantee. D5 and D8 demonstrate missed invalidations.

**Final test results**

Baseline `Rscript scripts/run_tests.R`: **PASS 115, FAIL 0**, 12 passing blocks.
Final `TESTTHAT_MAX_FAILS=Inf Rscript scripts/run_tests.R`: **PASS 338, FAIL 14, WARN 0, SKIP 0, ERROR 0**; exit status 1, intentionally.
There are **40 blocks: 29 passing and 11 failing**. The new file contains 17 passing regression guards and 11 failing defect blocks. The runner's “14 failed” counts assertions, not blocks.
`TESTTHAT_MAX_FAILS=Inf` is needed to complete this deliberately red suite instead of stopping at testthat's failure limit. Full output: [adversarial-run.log](tests/adversarial-run.log).

**Untested ideas / limits**

- Actual Connect Cloud worker eviction and filesystem retention; SQLite reopen and injected interruption were exercised locally, not a deployed restart.
- Real 25-phone/WebSocket load, browser-native number-input behavior, and end-to-end network timing; observer schedules were exercised headlessly.
- Multiple writer processes, OS-level simultaneous joins, every reset/delete crash boundary, disk-full/I/O failures, and abrupt power loss. The deployment's single-worker callbacks cannot interleave inside these synchronous functions.
