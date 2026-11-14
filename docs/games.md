# Games and card displays

## No-limit Texas Hold'em

```r
players <- list(rcards::player("Alice", points = 100),
                rcards::player("Bob", points = 100))
hand <- rcards::holdem_hand(players, seed = 42)
while (!hand$complete) {
  legal <- rcards::holdem_legal_actions(hand)
  action <- if ("check" %in% legal$actions) "check" else "call"
  hand <- rcards::holdem_action(hand, action)
}
hand$payouts
```

Seats are clockwise; the button posts the small blind heads-up. Otherwise the
next two seats post the blinds. The player after the big blind acts first
preflop; the first active player after the button acts first on later streets.
Hole cards are dealt clockwise in two rounds; community streets burn a card.

Actions are `fold`, `check`, `call`, or `raise`. A raise's `amount` is the **total
street contribution**, not an increment. Minimum full raises, short all-ins,
reopening betting, automatic board runouts, and folded-player eligibility are
enforced. Uncalled excess is refunded. Side pots are derived from actual
contributions; integer odd chips go to tied winners clockwise from the button.
The hand state includes private cards for engine inspection. Give policies only
`holdem_observation(hand)`, which excludes opponents' cards and the remaining pack.

## Equity and statistical bots

`holdem_equity(hole, board, opponents, equity_samples, seed)` samples unknown
opponent cards and remaining community cards without replacement. It returns
win/tie/loss frequencies, fractional-pot equity, sample count, and a Monte Carlo
standard error. Opponents are modeled with uniform random holdings, not inferred
ranges. Increasing the sampling budget improves precision at a runtime cost.

```r
bots <- list(rcards::poker_bot(risk = 0.4, adaptability = 0.2,
                              variation = 0.05, equity_samples = 128, seed = 1),
             rcards::poker_bot(risk = 0.7, adaptability = 0.4,
                              variation = 0.1, equity_samples = 128, seed = 2))
result <- rcards::simulate_poker_game(players, bots, hands = 3, seed = 42)
result$net_chips
```

- `risk` is baseline willingness to continue or raise near marginal equity.
- `adaptability` adjusts current risk toward a target based on the last 20 public
  opponent actions: greater aggression makes that target more cautious.
- `variation` adds bounded decision-to-decision noise; it does not change the
  persistent baseline.
- `equity_samples` controls calculation precision, independently of risk.

`poker_bot_action()` returns an updated bot; pass that object to later decisions
to retain its state. The simulation does this automatically, including across
hands. Each simulated hand starts from the supplied stacks, with a rotating
button. This is individual-hand simulation, not a tournament. Bot seeds and the
simulation seed make runs repeatable while preserving the caller's RNG state.

The policy compares estimated equity with pot odds plus a risk-dependent margin;
raise sizing also depends on risk. It is an explicit heuristic, not an optimal
strategy or a claim of playing strength. Contestable side-pot odds are approximate.
Custom policy functions can replace bots and receive only the safe observation.

## Configurable Mus

```r
game <- rcards::mus_game(c("Alice", "Bob", "Carol", "Dan"),
                        rules = rcards::mus_rules(kings = 8, points_to_win = 40),
                        seed = 42)
game <- rcards::mus_action(game, "cut")
while (!game$hand_complete) game <- rcards::mus_action(game, "pass")
game$scores
```

Partnerships are seats 1/3 and 2/4. A complete 40-card Spanish pack is constructed
in memory; no generated dataset is needed. The supported settings are four or
eight kings and a 30- or 40-point game. Eight kings maps threes to kings and twos
to aces in all comparisons. `mano` starts the hand and breaks ties clockwise.

A mus vote must be unanimous to exchange. `discard` accepts unique positions
(or no positions to stand pat); previous discards replenish a depleted stock,
excluding cards just discarded by that player. A cut begins the lances:

1. **Grande:** compare cards highest-first.
2. **Chica:** compare cards lowest-first.
3. **Pares:** duples (two pairs or four of a kind), medias, then a single pair.
4. **Juego:** 31, 32, 40, 37, 36, 35, 34, 33, in that order. If nobody has juego,
   play **punto**, where the largest total wins.

Face cards count ten for juego. Players without pairs/juego do not wager in that
lance; a single qualifying partnership receives its intrinsic points automatically.
All-pass grande/chica/punto award one point. Pares award one/two/three points per
winning-partnership hand with a pair/medias/duples; juego awards three for 31 and
two for another qualifying total. An accepted wager adds its stake to those
intrinsic points (punto's wager replaces its default one-point award).

`bet` starts at two points. `raise` names a total stake at least two above the
current offer. Only an opposing eligible seat responds. `accept` resolves the
lance; `reject` awards the previous offer, or one for the first refusal, plus
intrinsic pares/juego points to the partnership winning by refusal. An accepted
`ordago` resolves the entire game on the current lance. A rejected ordago follows
the ordinary refusal rule. These rules are explicit; house-specific real-31
exceptions, table signals, and multi-game match formats are not implemented.

Lances score in order. Reaching the configured target ends the game. Otherwise
`mus_next_hand(game, seed)` preserves scores and rotates mano after a hand ends.
`mus_legal_actions()` describes the next expected input. There is no Mus AI or
interactive console loop; callers supply the actions through the R API.

## English and Spanish displays

```r
rcards::translate_card(rcards::card("A", "Spades"), "es")$display
# "As de Picas ..." followed by the spade symbol
```

`translate_deck(pack, "en")` or `"es"` updates card displays and deck language,
including main/optional metadata. Values, suit identities, and symbols stay
unchanged, so poker and Mus rules are unaffected. Explicit translation replaces
custom display strings. Unrecognized labels are retained. Dataset files are
never rewritten by translation.
