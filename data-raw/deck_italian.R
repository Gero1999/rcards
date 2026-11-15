# Optional regeneration from the repository root after loading rcards.
# Do not run automatically during installation, tests, or game construction.
deck_italian <- italian_deck(language = "it")
usethis::use_data(deck_italian, overwrite = TRUE)
