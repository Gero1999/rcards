# Shared graphical table contract

`game_table_view(game, seat, language)` projects one fixed viewer's information
into reusable player seats, center zones, own hands, legal controls and public
results. A backed card contains only `back=TRUE`. The renderer never receives
the full game, controller state, shoe or unrevealed pile. Opponents reveal only
at legitimate engine-defined showdowns; Liar reveals only challenged batches.
Do not hand the full engine state to a browser.

`table_decision()` translates a selected action, own-hand positions and optional
amount/rank/capture option into the existing `game_action()` protocol. The engine
validates legality and owns all scoring. Poker raises remain TOTAL street bets;
Scopa selects an own card and a numbered legal capture; Mentiroso selects one or
more own cards and a rank. No user text is evaluated as R code.

The server-side session keeps one fixed human seat, independent of the current
bot actor. Bot steps receive only their own engine observations, are bounded,
and preserve caller RNG with per-session seeds. Invalid actions leave state
unchanged. Version checks reject clicks left over from an earlier turn. Games
with multiple deals stop at hand boundaries so results can be reviewed.

This is a local single-human interface, not network multiplayer or an
authorization sandbox. Custom controllers remain trusted R code. Console play
and simulations retain their existing interfaces.
