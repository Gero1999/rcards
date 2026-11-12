# Run from the repository root after loading rcards.
# The bundled dataset includes both optional jokers; deck() defaults to 52 cards.
deck_french <- rcards::deck(include_optional = TRUE)
save(deck_french, file = "data/deck_french.rda", compress = "xz", version = 2)
