# rcards

`rcards` is a development R package for constructing and manipulating playing
cards, decks, players, and game containers. Poker helpers are experimental;
a complete Texas Hold'em game or simulation is not available yet.

## Development installation

```r
install.packages(c("pkgload", "testthat"))
pkgload::load_all("/path/to/rcards")
```

The supported foundation API is `card()`, `deck()`, `player()`, `table()`,
`game()`, `shuffle()`, `discard_card()`, `take_cards()`, and `leave_cards()`.
The `table()` constructor masks `base::table()`; use qualified names when needed.
See function help for arguments. `deck()` creates an empty, generic deck.
Both bundled datasets are generated from explicit main and optional card lists
using `usethis::use_data()`: French cards have 52 standard cards and two optional
jokers; Spanish cards have 40 standard cards and eight optional 8s and 9s.
Use `deck(cards = attr(deck_french, "main_cards"))` for a 52-card French deck.
Pass `optional_cards` and `include_optional = TRUE` to add optional cards explicitly.

## Quick start

After loading the package:

```r
set.seed(42)
utils::data("deck_french", package = "rcards", envir = environment())
pack <- rcards::deck(cards = attr(deck_french, "main_cards"))
pack <- rcards::shuffle(pack)
alice <- rcards::player(name = "Alice")

dealt <- rcards::take_cards(alice, pack, n = 5)
stopifnot(length(dealt$player$hand) == 5L, length(dealt$deck) == 47L)

# Return two cards to the bottom of the remaining deck.
returned <- rcards::leave_cards(dealt$player, dealt$deck, c(1, 2))
stopifnot(length(returned$player$hand) == 3L, length(returned$deck) == 49L)
print(returned$player)
print(returned$deck)
```

`take_cards()` draws from the top of the deck; shuffle first for random dealing.
Operations return updated objects and do not modify their inputs. Deck slicing,
shuffling, discarding, drawing, and returning preserve deck class, language, and
active optional-card metadata. Invalid draw counts and return/discard positions
produce errors.

## Local validation

```r
testthat::test_local(filter = "card|deck|player|table|public-api|readme")
```

This runs the foundation checks, including the executable README example.
To run everything, use `testthat::test_local()`. The existing experimental poker
suite currently has failures in hand evaluation and pot settlement; a passing
foundation suite does not establish playable poker support.

Build and check the package from the parent directory:

```sh
R CMD build rcards
R CMD check --no-manual rcards_0.0.0.9000.tar.gz
```

These are local commands; no CI workflow is added.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) and [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).
