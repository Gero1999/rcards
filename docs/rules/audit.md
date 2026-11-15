# Rules audit evidence

Audit work last updated 2026-11-15. These are development checks, not certification by a
game rules organization.

## Executed rule-based checks

Poker rankings are checked with self-contained, hand-authored examples in
`tests/testthat/test-rules-audit.R`. Every adjacent category comparison names the
expected winner. Additional cases explain rank and kicker ordering, ace-low
straights, invalid ace wrapping, ties, and the expected best five from six or
seven cards. A shared-board royal flush explicitly checks a split result.
The examples use no external ranking table, generator, or extra dependency.
They are regression evidence, not exhaustive proof of all possible hands.
No R datasets were regenerated.

Existing and added scenarios also check original seat indices, betting order,
all-in tiers, uncalled refunds, odd chips, cumulative short raises, card
uniqueness, and chip conservation. Expected outcomes are hand-constructed from
the documented rules, not independently downloaded rule examples.

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
source review. Both requests were denied by the environment proxy on 2026-11-14.
Their text was **not retrieved or checked** during this audit. A network draft
adds only their hostnames alongside the existing GitHub API domain. Saving a
draft does not prove that access is applied. Refresh and retry before changing
this status or claiming the documented profile matches those publications.
