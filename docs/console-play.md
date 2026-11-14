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
`simulate_poker_game()`, including stateful policies across hands. Poker bots
support Hold'em; Mus currently supports console and scripted controllers.

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
