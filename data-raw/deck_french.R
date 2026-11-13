# Run from the repository root after loading rcards.
# French deck values and suits.
card_values <- c("A", as.character(2:10), "J", "Q", "K")
card_suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
base_cards <- unlist(lapply(card_suits, function(suit) {
  lapply(card_values, function(value) card(value, suit))
}), recursive = FALSE)

# Two optional jokers, kept separate from the 52 standard cards.
optional_cards <- list(
  card("Joker", "Joker", display = "Red Joker"),
  card("Joker", "Joker", display = "Black Joker")
)

deck_french <- deck(cards = base_cards, optional_cards = optional_cards,
                    language = "en", include_optional = TRUE)
usethis::use_data(deck_french, overwrite = TRUE)
