# Graphical play

Install `shiny` separately; it is an optional suggested dependency. From an
installed package or a checkout loaded with `pkgload::load_all()`:

```r
rcards::play_app()  # default: one Hold'em hand, You versus a bot
rcards::play_app(rcards::scopa_game(seed = 42), language = "it")
```

The browser uses a shared felt table, face-up own cards, covered opponents,
public center cards, legal action buttons, and short results. Card faces and
backs are built with local HTML/CSS. French suits use familiar glyphs, and
Spanish and Italian suits use local coin, cup, sword, and baton marks. There
are no external image or API requests. English (`en`), Spanish (`es`),
German (`de`), and Italian (`it`) are selectable during play.

## Games and controls

| Game | Default table options | Human controls |
| --- | --- | --- |
| Texas Hold'em | 2–10 seats; retained stacks across hands | Fold/check/call/raise. Raise **TO** a total for the betting round, including chips already committed. Bounds and the call amount are shown. |
| Mus | Four seats, two partnerships | Mus/cut, select any cards to discard, and legal lance/stake decisions. |
| Blackjack | One human versus the dealer; retained bankroll | Hit/stand/double/split when allowed. Split hands show their totals and wagers; the active hand is highlighted. |
| Scopa | Two or three individual players; four in teams | Select one own card, then a numbered capture option. Table positions and the chosen capture are highlighted. |
| Mentiroso | 2–8 seats, Spanish pack | Select one or more own cards and a declared rank, or pass/challenge when allowed. Only a challenged batch is revealed. |

During your Hold'em turn, **Call**, **Check**, and **Raise** sit beside the
board and covered deck. Choose **Raise** to open the amount field and see its
legal range, then use **Raise to** to confirm the total for the betting round.
**Fold** and **Hand odds** sit beside your hand. **Hand odds** opens a copy of
your cards and the public board. Choose **Calculate odds**
to see the chance of winning outright, tying for the best hand, or losing at
showdown. Unknown opposing hands and
future board cards are drawn uniformly. The flush result gives the chance
that your cards and the completed board contain five cards of one suit.
You can swap your cards or fill in
future cards to compare a what-if scenario, then reset to the captured hand.
Scenario edits do not affect play.

During your Blackjack turn, **Hit** and **Double** sit by the covered shoe.
**Stand**, **Split**, and **Compare actions** sit by your active hand.
**Double** and **Split** show the extra wager. The comparison estimates the
expected net profit in chips and the chance of finishing ahead for each action.
It samples unknown cards from the configured shoe and uses a baseline bot for
any remaining player decisions. The comparison includes additional wagers
from doubles or splits and shows the sampling interval for each estimate.

In Mentiroso, choose cards and declare their rank directly below your hand.
When it is your turn to respond, **Challenge** and **Pass** sit next to the
current claim and covered pile.

During your Scopa turn, **Compare moves** ranks each legal card and capture
combination by estimated point margin at the end of the current deal. It shows
the chance that your side finishes the deal ahead. The panel keeps your hand,
the table, and public captures fixed, samples plausible opposing hands and
stock, and uses baseline bots for the remaining moves. You return to the
same table and selection when you close it.

Changing settings takes effect on **New table**, which starts a fresh game
with the default rules and bots, seat one as the human, and fresh 100-chip
stacks where applicable. **Pause** stops automatic bot actions; **Resume**
continues. Bots take one action per timer tick. A bounded automatic-action
budget pauses unusually long play instead of looping indefinitely.

At a hand boundary, results stay visible for review. **Next hand** continues
poker with settled stacks, fixed seats, retained bot memory, and the dealer
rotated clockwise among funded players. Eliminated seats receive no cards or
blinds. The human table stops if your bankroll is empty or fewer than two
funded players remain; there are no automatic rebuys or stack resets.
Mus and Scopa retain scores and controller memory between hands. At the score
target, **New match** starts a rematch with the same players, rules, and bot
memory, explicitly resetting match scores to zero and rotating dealer/mano.
Mentiroso offers **Next deal** after the winner is confirmed, rotating the
dealer, clearing claims and reveals, and redealing the configured pack. It
does not invent a points or elimination system across deals.
`game_rematch(completed, seed = 42)` exposes these transitions programmatically;
`liar_next_deal(completed, seed = 42)` also supports an optional ordered pack.

Blackjack offers **Next round**, carrying settled bankrolls and controller
memory forward. The displayed next wager can be changed before continuing;
if the previous base wager exceeds your remaining bankroll, the field shows
the available balance for explicit confirmation. A zero bankroll stops play.
Bot seats unable to cover their own base wager sit out. Doubles and splits do
not increase subsequent base wagers. Each round uses a fresh complete shuffled
shoe; a continuous shoe and card-counting session are not implemented.
`blackjack_next_round(completed, wager = 5, seed = 42)` exposes the same engine
transition programmatically; `game_next_hand(completed)` keeps base wagers.

For programmatic or console poker, use `holdem_next_hand(completed, seed = 42)`
or `game_next_hand(completed)`, then call `play()` with the returned state and
the preceding play result's controllers. `game_complete()` still describes one
completed hand; advancing to another is explicit. The graphical session also
keeps up to twenty recent public completion summaries, never private hands.

## Configured games and local sessions

`play_app(game, seat, controllers, language, seed)` accepts an existing game,
a fixed human seat, and optional controllers for the other seats. The human
entry in the controller list is ignored; other seats require non-console
controllers. Supplying a game preserves its configured rules until **New
table**. Baseline bots are selected if controllers are omitted.

`play_app()` binds to `127.0.0.1` and opens a browser. Each browser connection
has its own in-memory game and controller state; browser refresh starts a new
session. There is no persistence, authentication, or network multiplayer.
Custom controllers run as trusted R code. Use `launch = FALSE` to obtain the
Shiny application object for local testing or a custom local launch:

```r
app <- rcards::play_app(launch = FALSE)
shiny::runApp(app, host = "127.0.0.1", port = 8765, launch.browser = FALSE)
```

Console play and automated simulations keep their existing interfaces. The
optional app adds no work to either path.

The header displays the original rcards logo. **Help** opens instructions for
the game actually at the table, an app overview, and links to report bugs or
suggest games. It follows the selected language and leaves game state intact.
See [in-app help](graphical-help.md) for details.

## Shared table contract

`game_table_view(game, seat, language)` projects one fixed viewer's information
into reusable player seats, center zones, own hands, legal controls and public
results. A backed card contains only `back=TRUE`. The renderer never receives
the full game, controller state, shoe or unrevealed pile. Opponents reveal only
at legitimate engine-defined showdowns; Liar reveals only challenged batches.
Do not hand the full engine state to a browser.

`table_decision()` translates a selected action, own-hand positions and optional
amount/rank/capture option into the existing `game_action()` protocol. The engine
validates legality and owns all scoring. Poker raises remain TOTAL street bets;
Scopa selects an own card and a numbered legal capture; Mentiroso selects one or
more own cards and a rank. No user text is evaluated as R code.

The server-side session keeps one fixed human seat, independent of the current
bot actor. Bot steps receive only their own engine observations, are bounded,
and preserve caller RNG with per-session seeds. Invalid actions leave state
unchanged. Version checks reject clicks left over from an earlier turn. Games
with multiple deals stop at hand boundaries so results can be reviewed.

This is a local single-human interface, not network multiplayer or an
authorization sandbox. Custom controllers remain trusted R code. Console play
and simulations retain their existing interfaces.

`R/table_view.R` owns game adapters and the allowlist of display information;
`R/table_session.R` owns transactional human/bot steps and controller memory.
`R/table_app.R` wires Shiny events to those steps. `R/table_render.R` and
`inst/app/` render the shared layout and controls. Recent actions contain only
allowlisted public fields, never actual hidden discards or unrevealed claims.

To add another game, first implement its engine/observation protocol and
document its rules as described in `AGENTS.md`. Add its view zones, action
mapping, default controller, player counts, result text, and translations.
Keep game-specific rules out of JavaScript and renderers. Add examples of
hidden-information boundaries, legal/illegal interactions, and hand completion
to the adapter and app tests before adding it to the game selector.

## Validation

`testthat::test_local()` includes adapter, RNG/state, information-boundary,
HTML-escaping, and Shiny server interaction tests. App tests skip if the
optional Shiny dependency is unavailable; install it to exercise them.

For the browser smoke test, start the default app locally on port 8765 using
the command above. In another terminal, with Python Playwright (1.48 or newer) and a local
Chromium installed:

```sh
python tools/check-table-app.py --chromium /path/to/chromium
```

The runner exercises all five game controls, Scopa move comparison, language
switching, and a narrow mobile viewport. It accepts only a loopback app URL,
blocks external HTTP requests, and reports browser errors. `--screenshots /tmp/rcards-preview`
optionally saves local previews. It uses newly generated local games and
does not require credentials or external services.

## Session scoreboard

Session history counts every completed hand/deal, including the current result
before continuing. Wins mean positive net profit in poker/Blackjack and confirmed
match/deal victories in Mus/Scopa/Mentiroso. Partners share match wins. Bankroll
includes chips committed to the current hand, so posting blinds is not a loss.
Only the latest twenty public results are retained; counters cover the whole
session. New table and browser refresh reset the session.

Card selection shows a live count against the current legal maximum. Selected
hand cards use mint outlines; a Scopa capture uses gold table-card outlines.
Keyboard focus has a separate white outline. Paused controls explain that play
must be resumed, and action targets have a minimum height of 44 pixels.
