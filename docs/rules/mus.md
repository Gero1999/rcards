# Supported Mus rules

Read this before editing `R/mus.R` or a Mus controller.

## Configuration

Use a 40-card Spanish pack: 1-7 and 10-12 in Oros, Copas, Espadas, Bastos.
Four clockwise seats form partnerships 1/3 and 2/4. Mano leads and breaks equal
hands clockwise. The supported king counts are four or eight; in eight-kings
play threes count as kings and twos as aces in every comparison. The game target
is selectable as 30 or 40; default eight kings and 40 points. No real-31 house
exception, table signals, or multi-game match format is implemented.

## Mus and the lances

Deal four cards per player. All four must request mus to exchange; any cut ends
exchanges. Each seat then discards selected positions or stands pat. Replenish
an exhausted stock from previous discards while excluding the cards just
returned by the drawing player.

Negotiate grande, chica, pares, then juego; use punto when nobody has juego.
Grande compares ranks descending; chica compares lowest-first. Pares order is
duples (four equal or two pairs), medias, single pair. Four equal count as
high/low duples of that rank. Juego order is 31, 32, 40, 37, 36, 35, 34, 33.
Face cards count ten; punto compares totals below 31.

Only qualifying players participate in pares/juego wagering. One qualifying
partnership receives its intrinsic points without negotiation. The first bet is
at least two; the current software profile requires total raises at least two
above the preceding offer. Counteroffers alternate partnerships. This minimum
raise is an explicit supported-profile choice pending external-source review.

## Scoring order is part of the rules

- Accepted wagers and passed-lance comparisons are settled **after** all lances
  have been negotiated, in grande/chica/pares/juego-or-punto order.
- A refusal immediately awards the previous offer, or one point when the first
  offer is refused. Intrinsic pares/juego points wait for hand-end settlement.
- All-pass grande/chica/punto award one. Pares intrinsic points per winning-team
  hand are one for a pair, two for medias, three for duples. Juego gives three
  for 31, two for another qualifying total. Add an accepted wager to pares/juego
  intrinsic points; an accepted punto wager replaces the one-point default.
- An accepted ordago decides the game on that lance immediately. Earlier
  unresolved accepted wagers must not preempt it. Refusing ordago follows the
  ordinary refusal rule. Already-scored refusal points can end the game first.
- At hand-end settlement stop as soon as a team reaches the target. Do not award
  later-lance points after the game has ended.

Do not expose queued winners through observations: they depend on hidden cards.
Show hands only at hand-end comparison or an accepted ordago; an early
score-target victory by refusal does not require card revelation.

## Interface, evidence and reference

`mus_game` inherits from `game`; human and scripted controllers use `play()`.
`mus_observation` contains only the seat's own hand and public data, replacing
private discard positions with counts. Public pares/juego declarations are
available at the relevant lance. Next hands rotate mano and retain scores.

Regression scenarios live in `test-mus.R`, `test-rules-audit.R`, and
`test-game-protocol.R`. See [audit status](audit.md) for confirmed code checks and
outstanding source verification. Reference for variant comparison:
[Pagat Mus rules](https://www.pagat.com/vying/mus.html). Do not silently introduce
house rules or describe these links as consulted when access was blocked.

## Baseline controller

`mus_bot()` uses bounded public-information sampling for partnership lance
strength, then explicit exchange/wager heuristics. It samples unknown holdings
uniformly; it is not an optimal strategy or inference from signals. Current
pares/juego declarations constrain accepted candidates. Sampling attempts are
bounded and zero accepted samples produce a documented neutral estimate.
Risk, adaptation and variation are separate from sampling budget. The bot cuts
after at most two exchange rounds and bounds autonomous raises by the score
target. Test all chosen actions through the engine, never trust heuristics to
change rules. Future improvements can model discards/opponent ranges explicitly.
