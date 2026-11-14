# Supported no-limit Texas Hold'em rules

Read this before editing `R/holdem.R`, poker scoring, equity, or bots.

## State and cards

- Two to ten clockwise seats; each funded stack and chip movement is an integer.
- Construct 52 distinct French cards explicitly, without jokers. Generic `deck()`
  stays empty. Deal two rounds beginning left of the button.
- Use the best five of seven at showdown; both, one, or neither hole card may
  play. Aces can be high or low in A-2-3-4-5; the wheel is five-high.
- Rank straight flush, quads, full house, flush, straight, trips, two pair, pair,
  high card. A royal flush is an ace-high straight flush. Compare kickers in
  category-specific order, not by the sum of card ranks.

## Turns and wagers

The next two seats post small/big blinds; heads-up the button posts the small
blind. The first active seat after the big blind acts preflop; after the button
acts first on subsequent streets. Burn a card before flop, turn, and river.

A decision is fold, check, call, or raise. Raise amounts are total street
contributions. Full raises establish the minimum increment. Short all-ins may
force calls without reopening previous actors' raises; cumulative short raises
can reopen betting once a full increment has been faced. A lone player with
chips need only resolve a call; otherwise the board runs out automatically.

Pot tiers derive from actual contributions. Folded players cannot win contested
pots. Return uncalled excess separately from wins. Split tied pots in integer
units, giving odd chips clockwise from the button among tied winners. Assert
that final stacks equal initial chips and that no card or chip is duplicated.

## Completion and information

A hand ends when everyone else folds or the last street settles. Reveal live
hands only at showdown; winning by folds does not reveal cards. Controllers see
only their own hole cards, public actions, board, and stacks. The engine object
is full in-process state, not a security sandbox.

`holdem_hand` is a `game` subtype. Use `game_*` methods and `play()` for human or
scripted play. The simulation runs independent hands with fresh starting stacks;
it is not a continuous cash session or tournament. Bot state can persist.

## Evidence and references

- `tests/testthat/test-holdem.R`: acting order, all-ins, side pots, odd chips,
  reopening, and varied legal-action conservation scenarios.
- `tests/testthat/test-rules-audit.R`: 180 independent five/six/seven-card
  comparisons from Treys 0.1.8. Generator: `tools/generate-poker-oracle.py`.
- Reference: [Poker TDA rules](https://www.pokertda.com/poker-tda-rules/).
- Independent rank implementation: [Treys](https://github.com/ihendley/treys).

See [audit status](audit.md) before claiming verification against external rule
publications. Betting policies should be checked against specific cited examples
when source access is available; tournament administrative penalties and live
show/muck procedures are outside this local software engine.
