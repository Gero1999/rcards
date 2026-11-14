# rcards development

Use the existing task checkout. Do not create a worktree unless requested.

## Start with the rules

- Read [Hold'em rules](docs/rules/holdem.md) before editing poker, equity, or betting.
- Read [Mus rules](docs/rules/mus.md) before editing Mus or its controllers.
- Read [audit evidence](docs/rules/audit.md) to distinguish checked behavior,
  reference links, explicit house choices, and unresolved source verification.
- Document rule changes and add concrete regression examples before changing
  the engine. Keep rules readable; prefer named intermediate values to dense
  expressions. Scores are comparison encodings, not chip or game-point awards.

## Adding a game

1. Add `docs/rules/<game>.md`: deck, players/partnerships, turn order, actions,
   transitions, scoring, ties, completion, variants, sources, and supported limits.
2. Build an S3 state inheriting from `game`. Keep full engine state separate from
   players' observations and controller memory.
3. Implement `game_observation`, `game_legal_actions`, `game_action`,
   `game_complete`, `game_hand_complete`, and `game_result`. Implement
   `game_next_hand` when a game spans hands. A plain `game()` is only a container.
4. Return action lists through `choose_action`. Human, bot, and scripted
   controllers use the same rules and legal-action validation. A bot must work
   from its supplied observation, never from another seat's cards or the deck.
5. Add a game-specific console view/result renderer; the current renderers know
   Hold'em and Mus. A shared state protocol alone does not define a new game's UI.
   Put display work in console code. Rich cards and winner messages belong to
   active human play; automated simulations remain silent and return data.
6. Add seeded scenarios, illegal-action tests, information-boundary checks,
   conservation tests, and an example using exported APIs. Add independent
   expected outcomes; avoid only comparing two helpers using the same logic.

## Data and interfaces

- `deck()` is generic and empty by default. Construct game-specific packs
  explicitly. French and Spanish dataset generators use `usethis::use_data()`.
- Do not regenerate or commit `.rda` files without explicit user instruction.
- Preserve canonical card values and suits when translating displays.
- Reveal cards only at a legitimate showdown. Folded poker cards and early
  concessions remain hidden. Public results must not leak pending Mus winners.
- Keep simulations reproducible and preserve the caller's RNG when a seed is
  supplied. Controllers retain their own state across decisions.

## Validation and delivery

Run `roxygen2::roxygenise()` for public documentation/export changes, then
`testthat::test_local()`, `R CMD build`, and `R CMD check --no-manual` for engine
or public-interface changes. Report passed, failed, skipped, and unrun checks
separately. Do not add CI unless requested. Readable console output needs an
interactive R terminal check as well as injected-input tests.

Use the provided `username` and `noreply` environment variables for both Git
author and committer identity without displaying their values. If unavailable,
resolve identity before committing. Preserve existing work. PR descriptions
should explain the changes concisely, without branch-reference or cumulative
history instructions. Keep the hex logo outside the current scope.
