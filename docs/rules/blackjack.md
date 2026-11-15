# Blackjack supported profile

One to seven players against a dealer, one round per `blackjack_game()`. Each
player supplies a wager; the dealer is not a controller seat. Use explicit French
packs without jokers. Default six decks; one through eight decks configurable.
A supplied ordered pack must contain exactly the configured number of each card.

Aces count eleven unless reducing them to one avoids a bust. Naturals require
exactly two original cards, pay 3:2 by default, and beat ordinary 21. Dealer checks
for a natural before player decisions; both naturals push. Default dealer stands
on soft 17; hitting soft 17 and a 6:5 natural payout are configurable.

Hit, stand, double on an initial two-card hand (any total), and split equal ranks.
Doubling draws one card then stands. Default maximum four hands per seat;
resplitting aces is disabled. Split aces receive one card each and stand. A split
21 is ordinary 21, never a natural. Double after non-ace splits is configurable.
A player must have funds for every additional wager. Wagers are escrowed;
settlement returns stakes and winnings, including pushes. Fractional chip values
are allowed for payouts. No insurance, surrender, continuous shoe, or card-counting
strategy is implemented. A round errors if the supplied shoe is exhausted.

Dealer hole card and shoe remain hidden in observations until settlement. During
player turns only the observing seat's hands and other seats' hand counts are
included. At settlement all played hands and dealer cards may be revealed.
Bots use only public observations. They are baseline heuristics, not certified
basic strategy. `play()` completes one round; create a new round explicitly.

## Continuing at the same table

After settlement, `blackjack_next_round(game, wager = NULL, seed = NULL)` or
`game_next_hand(game)` deals a new round with the settled bankrolls, player
metadata, seat indices, and configured rules retained. `round_number` increases.
The default wager is the original base wager for each seat, not a doubled or
split hand's final stake. An explicit positive scalar wager applies to every
seat; a vector sets a positive wager for each original seat.

Seats that cannot cover their chosen wager sit out with no cards and keep their
bankroll. They can rejoin a later round if an explicitly lower wager is affordable.
At least one seat must cover its wager. The initial `blackjack_game()` constructor
still requires every initial player to be funded. No chips are added automatically.

Every continued round shuffles a fresh complete shoe with the configured number
of decks. It does not carry the previous shoe or support card counting. Only a
settled round can continue; settlement is never applied twice. For example, a
100-chip player who wins a 10-chip doubled wager retains 120 chips, then a new
10-chip base wager leaves 110 chips available during the next round. A 5-chip
bankroll sitting out a 10-chip game can explicitly choose a 5-chip wager.

Tests use original, hand-constructed examples, not third-party lookup tables.
