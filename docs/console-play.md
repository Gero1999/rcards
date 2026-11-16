# Console play and the shared game interface

Players hold game information: name, seat, cards, and chips or partnership scores.
Controllers make decisions: a human, a poker bot, or a scripted strategy. The
rules engine determines turns, legal actions, transitions, and results.

`holdem_hand()` and `mus_game()` are subclasses of `game`. The original `game()`
constructor remains a general container; it does not implement particular rules.

## A human against a poker bot

Run in an interactive R session:

```r
players <- list(rcards::player("You", points = 100),
                rcards::player("Bot", points = 100))
played <- rcards::play(
  rcards::holdem_hand(players, seed = 42),
  list(rcards::console_player(),
       rcards::poker_bot(risk = 0.6, equity_samples = 128, seed = 1))
)
```

Every prompt shows your own cards, the board, stacks, pot, call amount, recent
public actions, and legal actions. Enter an action name or its menu number:

- `call`, `check`, or `fold` when listed.
- `raise 10` means your **total contribution on this street** is ten, not a raise
  by ten. The prompt displays legal lower and upper bounds. Entering `raise`
  alone prompts for an amount.
- `q`, `quit`, or `exit` stops. EOF also stops; Ctrl+C during input returns an
  interrupted result. Accepted actions remain in the returned game.

Invalid human input is explained and retried at the same turn. No game state is
changed by an invalid answer. Invalid bot or policy decisions raise errors so
programming mistakes are visible rather than silently retried.

For example, if the current bet is 10 and you have already contributed 4 this
round, `raise 20` brings your total to 20 and takes 16 more chips from your stack.
The total is for this betting round, not the whole hand or pot. Amounts outside
the displayed bounds are rejected without changing the state, and you are
prompted again. Enter `call` to match the current bet; an undersized raise is not
automatically converted to a call. The displayed minimum accounts for a legal
short all-in when your stack cannot cover a full raise.

`poker_bot(equity_samples = 128)` samples 128 random opponent-card and board
completions per decision to estimate the bot's expected share of the pot.
Increasing this number reduces sampling noise but takes longer; it does not
change risk tolerance or make the strategy optimal. The samples are calculated
locally, without network or API calls.

## Four-player Mus

```r
mus <- rcards::mus_game(c("Alice", "Bob", "Carol", "Dan"),
                       rules = rcards::mus_rules(kings = 8, points_to_win = 40),
                       seed = 42)
controllers <- rep(list(rcards::console_player(language = "es")), 4)
played <- rcards::play(mus, controllers)
```

The active seat gets its own four cards, partner, mano, phase, score, stake, and
legal actions. Use `mus` or `cut` for the vote, `discard 1 3` to exchange positions
one and three, and a bare `discard` (or `discard none`) to stand pat. Bets and
raises specify total stakes, such as `bet 2` and `raise 4`. The engine handles
acceptance, refusal, ordago, and scoring through the existing Mus rules.

A shared terminal retains earlier output: console controllers limit their
current view, but do not provide separate private screens for four humans.

By default, `play()` stops after a single hand. To continue Mus through its
configured score target, use `until = "game"`. The dealer/actor progression and
scoring remain in the Mus engine; the console only collects decisions.

## Stopping, resuming, and results

```r
played$status
played$result  # public summary
played$game    # full engine state; do not pass this directly to a controller

# Resume at the saved turn, preserving bot/policy state:
played <- rcards::play(played$game, played$controllers)

# Advance a finished Mus hand with retained scores:
next_hand <- rcards::game_next_hand(played$game)
```

`game_next_hand()` requires a finished Mus hand whose score target has not been
reached. For automatic advancement, call `play(..., until = "game")` instead.
Hold'em currently represents one hand, not an ongoing poker session.

Statuses are `complete`, `hand_complete`, `quit`, `interrupted`, `action_limit`,
and `input_limit`. `max_actions` caps accepted actions per call; `max_invalid`
caps consecutive invalid console answers for one turn. Limits return the current
state. Poker simulation treats an incomplete hand as an error rather than counting
it as a completed simulation.

## Stateless and stateful policies

A plain function receives an observation and returns an action list:

```r
passive <- function(observation) {
  list(action = if ("check" %in% observation$legal$actions) "check" else "call")
}
```

For a controller with its own memory:

```r
counter <- rcards::policy_player(function(observation, state) {
  action <- if ("check" %in% observation$legal$actions) "check" else "call"
  list(decision = list(action = action), state = state + 1L)
}, state = 0L)
```

`choose_action(controller, observation)` returns `decision` and an updated
`controller`. The runner saves updates after accepted actions; reuse
`played$controllers` to retain them. The same protocol drives
`simulate_poker_game()`, including stateful policies across hands. Poker bots support Hold'em; Mus supports console, scripted controllers, and
`mus_bot()`.

## Scripted console input and output

`console_player(read = ..., write = ...)` accepts callbacks. `read(prompt)`
returns one character line; `write(text)` receives display text. This permits
repeatable tests or another UI without adding input/output to the rules engine.
The default reader requires interactive R and does not silently hang an Rscript.

```r
inputs <- c("invalid", "q")
index <- 0L
human <- rcards::console_player(
  read = function(prompt) {
    index <<- index + 1L
    if (index > length(inputs)) return(character()) # EOF
    inputs[[index]]
  },
  write = function(text) cat(text, "\n")
)
```

## Game interface and information boundaries

Both engines implement:

- `game_observation(game, seat)` — own cards and public information; defaults to
  the actor. At completion, an unspecified seat produces a public-only view.
- `game_legal_actions(game)` — next actor and permitted actions/constraints.
- `game_action(game, decision)` — validate an action and return new state.
- `game_complete(game)` — entire game finished: one Hold'em hand or a Mus score target.
- `game_hand_complete(game)` — also identifies a Mus hand boundary.
- `game_result(game)` — public stacks/payouts or partnership scores/results.
- `game_next_hand(game)` — Mus-only advancement preserving scores.

Decisions contain `action` and optional `amount` or `cards`. Turn/amount/discard
validation is delegated to the existing engines. Mus observations replace
private discard positions with public discard counts and expose pair/juego
eligibility only at the relevant lance. Opponent cards and deck order are never
passed to controllers by the runner. Full game objects remain inspectable by
code running in the same R process; this projection is not an isolation sandbox.

## Card-first display

Active console controllers render compact card faces, covered opponent hands,
and a short winner/split/refund line at the end. Live poker hands are revealed
only at contested showdown; folded cards and hands won by folds stay private.
Mus cards are shown at hand-end settlement or accepted ordago, not an early
refusal victory. Completed Mus hands are displayed before the next deal when
playing a whole game. Other controllers and automated simulations do no rendering.

Use `console_player(style = "ascii", width = 40)` for narrow/basic terminals;
`auto` chooses Unicode borders on a UTF-8 terminal. Cards wrap to fit the selected
width. Spanish suits use O/C/E/B on card faces; an explicitly selected language
adds a compact suit-name legend. Simulation results remain structured objects.

## One human with three Mus bots

```r
game <- rcards::mus_game(c("You", "Opponent bot", "Partner bot", "Other bot"),
                        rules = rcards::mus_rules(kings = 8, points_to_win = 40),
                        seed = 42)
controllers <- c(list(rcards::console_player()),
                 lapply(1:3, function(i) rcards::mus_bot(seed = i)))
played <- rcards::play(game, controllers)
```

Seat 3 is your partner; seats 2 and 4 are opponents. Use `until = "game"` to play
to the target score. Bot states are returned in `played$controllers`. Their
`last_estimate` records actual accepted samples and attempts; public declaration
constraints can reduce the sample count. This initial controller uses uniform
unknown holdings and simple strategic rules, not a claim of expert Mus strength.

## Four console languages

Choose `console_player(language="en")`, `"es"`, `"de"`, or `"it"`. Prompts,
legal-action labels, expected input errors and result messages are localized;
English action identifiers and menu numbers are always accepted. User names,
internal actions and game rules are unchanged. Missing translations and external
controller diagnostics fall back to English. See [language policy](languages.md).

## Blackjack

```r
played <- rcards::play(
  rcards::blackjack_game(list(rcards::player("You",points=100))),
  list(rcards::console_player(language="it"))
)
```

Use hit/stand/double/split (or their displayed translations). The active split
hand and wager are displayed explicitly. Dealer play and settlement are automatic.
This is one round, not a continuing casino session.

## Multiplayer Scopa

```r
played <- rcards::play(
  rcards::scopa_game(c("You","Bot 1","Partner","Bot 2")),
  c(list(rcards::console_player(language="it")),
    rep(list(rcards::scopa_bot()),3)),
  until="game"
)
```

Cards on the table and in your hand are numbered independently. Enter
`play 2 1` to play your second hand card using its first listed capture option.
`play 2` prompts when multiple captures exist; bare `play` prompts for the hand
position first. Captures list table positions, not card values. `q` pauses.
Two- and three-player games use the same interface with fewer controllers.

## Mentiroso (Liar)

```r
played <- rcards::play(
  rcards::liar_game(c("You","Bot 1","Bot 2")),
  c(list(rcards::console_player(language="es")),
    rep(list(rcards::liar_bot()),2))
)
```

Enter `claim 7 1 3` to discard your first and third cards while claiming sevens.
The displayed declared rank must be used until someone challenges; cards may be
a bluff. Bare `claim` prompts for the opening rank (when needed) and positions.
English commands always work. Spanish uses `declarar`, `mentiroso`, `pasar`;
German uses `ansagen`, `anzweifeln`, `passen`; Italian uses `dichiarare`,
`bugiardo`, `passare`. Use q to pause and resume with the returned controllers.

After a claim, opponents respond clockwise with challenge/pass. A challenge
reveals only the latest batch; it never reveals earlier face-down cards in the
pickup. Last-card claims are provisional until responses finish. As with other
games, shared terminal history is not a private screen for multiple humans.

For French packs, Q (or q) at a rank prompt means Queen. Type `quit` to exit
that prompt; q still exits ordinary action/card-position prompts. Inline
`claim Q 1` also works. This avoids confusing the Queen rank with the quit key.

## Continuous console sessions

Use `play_session(game, controllers)` to review each completed hand and choose
whether to continue. It preserves stacks, rules and updated controllers. The
first console seat confirms continuation; yes/si/sí/sì/ja are accepted. Blank,
no, quit, EOF or an interrupted confirmation stops without dealing another hand.
`max_hands` and per-hand `max_actions` bound automatic work. A completed Mus or
Scopa match asks for a new match; its scores then reset explicitly. Blackjack
shows the next wager and uses a fresh shoe. The returned game and controllers
can be supplied to a later call; public session summaries are in session_results.
The existing `play()` function keeps its single-call behavior.
