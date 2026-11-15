# Statistical estimation contract

Estimates use public observations, never the actual hidden hands or future deck
order. Card-only strength, expected action profit, and strategy-dependent game
outcomes are different questions. Reports state method, assumptions, sample
counts, and 95% uncertainty intervals. Sampling intervals quantify Monte Carlo
noise only, not wrong assumptions about players or missing information.

`draw_probability(successes, remaining, draws)` computes the exact chance of at
least one success in uniform draws without replacement, without simulation.
It is useful for known outs, not a substitute for showdown equity.

Probability intervals use Wilson intervals, so zero observed successes does not
mean a proven zero probability. Equity intervals use the bounded-mean Hoeffding inequality, remaining nonzero
even if every sampled outcome is identical. Profit/point means use a normal
Monte Carlo approximation, which can be unreliable for small samples or no
observed variation; one sample has no estimated standard error. Exact enumeration
has zero sampling error but still depends on its stated model. No API calls,
external lookup tables, or binary datasets are required.

## Hand and action estimates

- `poker_equity(hole, board, ...)`: `method="auto"` enumerates when the state
  count is within `max_exact`, otherwise samples `n` completions. Known opposing
  hands may be supplied explicitly. `method="exact"` rejects an excessive state
  space instead of silently overrunning the budget. `holdem_equity()` and
  `hand_chances_poker()` remain available unchanged.
- `estimate_hand(game_observation(game), ...)` dispatches to Poker equity or
  `mus_lance_equity()`. Mus reports conditional comparison wins by lance,
  applicability, attempts and rejection exhaustion, never match-winning odds.
- `estimate_actions(game_observation(game), n=20, seed=42)` supports Blackjack
  and Scopa. It evaluates each legal choice, then continues with baseline bots
  or explicitly supplied controllers. It does not inspect the actual hidden
  cards and does not mutate the live game.

Blackjack uses a **single-seat model**, settling all the acting player's own
hands against the dealer. Other seats and their future choices are omitted.
Unknown cards are uniform after removing visible own cards and the dealer
upcard, with the hole card conditioned on the completed dealer-natural check.
Profit includes original escrowed wagers and additional double/split stakes.
It is a strategy-dependent Monte Carlo estimate, not exact basic strategy.

Scopa reconstructs public capture piles and the last capturer from history,
keeps the acting hand/table fixed, then uniformly allocates the unseen cards.
It ignores inference from earlier player choices. Continuations finish the
current deal. Reported margin compares the side's final deal points with the
best other side; a deal lead is not a match-winning probability.

Action reports reuse initial hidden allocations across candidates and reset
controller state per rollout. External mutable policy closures are not isolated.
`max_evaluations` bounds samples times candidates; `max_actions` bounds each
continuation. An unfinished continuation aborts rather than becoming a loss.

## Repeated independent games

```r
factory <- function(i) blackjack_game(list(player("You", points=100)))
results <- simulate_games(factory, list(blackjack_bot()), n=100, seed=42)
results$summary
```

Supply a fresh-game factory with no hard-coded shuffle seed to sample a varied
population. A constant seed deliberately studies the same initial scenario.
Controllers can be a list or `function(game, i)` producing fresh controllers.
They receive engine observations when acting; factories/controller callbacks
are trusted R code, not sandboxed. Avoid console controllers and external state
if you need independent, unattended, reproducible trials.

For Poker/Blackjack, outcome win/tie/loss means positive/zero/negative **net chip
profit**, not showdown victory, a shared pot or a Blackjack hand-level push.
For Mus/Scopa `until="hand"`, outcomes compare side points gained in that deal.
With `until="game"`, they use the completed match winner. These definitions are
included in every report. Partial runs error by default. With
`on_incomplete="record"` they are excluded from every denominator and listed
separately; exclusion can introduce bias. Full hidden game states are not
retained unless `keep_games=TRUE` is explicitly requested.

## Choosing a cost budget

Use exact draw counting for simple outs. Prefer showdown/lance sampling over
complete betting simulation when the question is about card strength. Exact
Poker enumeration is useful late in a hand or with known opposing cards; its
budget is a state-count limit, not a promise of wall-clock latency. Begin with
1000 card samples or 20–100 action rollouts, then inspect uncertainty and runtime.

Monte Carlo work grows roughly linearly with sample count; four times the
samples roughly halves standard error. Full-game simulation can be much more
expensive because bots may sample internally on every turn. For controller
comparisons, use modest internal `equity_samples` before increasing outer runs.
Blackjack profit and Scopa action estimates currently use rollouts; exact
Blackjack dynamic programming and learned opponent ranges are future work.

An optional local benchmark is installed at `benchmarks/statistics.R`. Run
`source(system.file("benchmarks", "statistics.R", package="rcards"))` to compare
small exact/sample/rollout workloads. It reports elapsed seconds and evaluated
samples/runs; these workloads differ and are not interchangeable accuracy tests.
No network calls or extra benchmark packages are used. For bots that retain
learning across Poker hands, the older `simulate_poker_game()` deliberately
carries controller state; `simulate_games()` instead resets it each run.

Action estimation requires an active acting-seat observation. A Blackjack
constructor can settle naturals immediately; if the round is already complete,
there are no actions to estimate. Mus estimates condition on the current own
hand and declarations only; earlier discards are not modeled.

Blackjack prepares the visible-card shoe model once per estimation call and
reuses it across trials; this local reuse never caches hidden engine state or
answers across users. The default action budget is 20 samples per candidate: a
quick exploratory estimate, not high precision. Increase n when needed.

Mentiroso/Liar is supported by `simulate_games()` with non-console controllers.
It reports confirmed winners and negative remaining-card counts, not hand
strength or a calibrated probability that a claim is a bluff. Retain action
limits: some strategy combinations can cycle. No dedicated Liar hand/action
estimator is provided.
