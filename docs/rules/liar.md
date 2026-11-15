# Mentiroso (Liar): supported profile

This implements Mentiroso, not the A-to-K cycling Cheat variant or Liar's Poker.
Two through eight individual players; no partnerships. Default forty-card Spanish
pack (1–7, 10–12 in Oros/Copas/Espadas/Bastos). A French 52-card pack without
jokers is optional. Use one complete pack; every rank has four copies. Deal all
cards clockwise, beginning left of the dealer; uneven hands are allowed.

The opening player chooses any rank in the configured pack and discards one
through four cards face down, claiming all have that rank. Subsequent players
must claim the same rank until a challenge. Cards can be truthful or a bluff;
there is no pass instead of discarding. Rank choices and the per-claim maximum
are explicit rules. max_cards can be set to one through four.

After each claim, every opponent gets one clockwise response opportunity:
challenge or pass. The first challenge ends the response window. Passing here
means declining a challenge, not skipping a discard turn. After all decline,
play continues left of the claimant, keeping the same rank and face-down pile.

A challenge exposes ONLY the latest claimed batch. If any card is off-rank,
the claimant picks up the entire pile; otherwise the challenger picks it up.
That pickup player leads the next empty pile and chooses a new rank. Earlier
unchallenged batches stay hidden, even when picked up. Everyone knows the latest
revealed batch; other cards in the pickup remain private.

An empty-handed claimant wins only after the response window closes, or after
surviving a challenge. A caught bluff on the last cards cannot win. A game is
one deal, without scores or elimination rounds. There is no mathematical turn
limit in the rules; play() action limits can pause potentially long games.

Observations expose own cards, other hand counts, pile count, current rank,
public claim rank/count/claimant, response order, and public challenge reveals.
Never expose opponents' hands, unrevealed batches, discard positions or a hidden
truth flag. The full engine object remains private in-process state, not a
security sandbox. Tests use original hand-constructed examples and no external
ranking data or copied assets. These are explicit software rule choices, not
certification against a regional rule publication.
