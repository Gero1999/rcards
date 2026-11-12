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
See function help for arguments. Deck and player operations are being stabilized.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) and [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).
