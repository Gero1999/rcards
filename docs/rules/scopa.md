# Scopa supported profile

Use a 40-card Italian pack: ranks 1 through 10 in Denari, Coppe, Spade, Bastoni.
Ranks 8, 9, 10 are fante, cavallo, re; their capture values are 8, 9, 10.
Two or three players play individually; four play fixed partnerships 1/3 and 2/4.
Deal clockwise, beginning left of the dealer. Deal three cards per seat and four
to the table, then replenish empty hands three at a time until the stock is empty.
The same player count is used throughout a match. Three-player games divide the
remaining 36 cards into four batches. No cards are removed.

Play one hand card. A same-value single table card MUST be captured if available;
otherwise capture a subset summing to the played value. Choose among all legal
captures. If none exists, leave the played card on the table. No bonus for the
seven of coins beyond its ordinary scoring point. A sweep scores one except on
the final play of the deal. The last capturer receives leftover table cards,
without a sweep bonus. A deal with no captures leaves the table unawarded.

Deal scoring: one point for most cards, most Denari, settebello (7 Denari), and
best primiera, plus sweeps. Tied majorities/primiera earn no point. Primiera uses
the best card per suit: 7=21, 6=18, ace=16, 5=15, 4=14, 3=13, 2=12, faces=10.
A team missing a suit is ineligible for primiera. In four-player play combine
partners' captures and sweep points before scoring.

Default match target eleven; configurable positive integer. Finish the deal
before comparing scores. A tied lead at/above target requires another deal;
otherwise the unique leading side wins. Rotate dealer between deals. No initial
three-king redeal or regional bonus variants are included in this profile.

Observations expose the table, own cards, public capture history and scores,
other players' hand counts, and stock count. Never expose opponents' hands or
future stock order. Console moves select a hand position and a numbered capture
option. A baseline bot chooses only among legal public capture options.

These are explicit software rule choices, not a claim of verification against
external publications. Tests use self-contained, hand-authored examples.
