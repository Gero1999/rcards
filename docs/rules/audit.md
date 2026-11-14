# Rules audit evidence

Audit work dated 2026-11-14. These are development checks, not certification by a
game rules organization.

## Executed independent checks

Poker rankings were compared with Treys 0.1.8, an independent implementation.
The checked-in text fixture contains 180 deterministic pairwise comparisons:
60 each for five, six and seven cards. Treys uses lower-is-better ranks;
rcards uses higher-is-better scores. Tests compare the ordering, not the encodings.
Regenerate only with the optional development generator and that pinned oracle;
Treys is not a package/runtime dependency. No R datasets were regenerated.

Existing and added scenarios check wheel handling, tie breakers, original seat
indices, betting order, all-in tiers, uncalled refunds, odd chips, cumulative
short raises, card uniqueness, and chip conservation. Some are hand-constructed
expected outcomes, not independently downloaded rule examples.

## Mus finding and correction

Accepted wagers previously awarded points before later lances were negotiated.
A team on 39 could incorrectly win on an accepted grande wager before a later
accepted chica ordago. Tests now require deferred settlement and the later
ordago outcome. Refusal points remain immediate, while intrinsic pares/juego
points are queued. Settlement stops at the first game-winning lance.

The supported profile is explicit in `mus.md`. Variants such as a real-31
exception and alternative minimum-raise rules require separate rule decisions.

## External publication access

The Poker TDA and Pagat URLs in the rule documents are references for subsequent
source review. Both requests were denied by the environment proxy on this date.
Their text was **not retrieved or checked** during this audit. A network draft
adds only their hostnames alongside the existing GitHub API domain. Saving a
draft does not prove that access is applied. Refresh and retry before changing
this status or claiming the documented profile matches those publications.
