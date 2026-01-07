# Define suit symbols
suit_symbols <- c(
	Hearts = "\u2665",   # ♥
	Diamonds = "\u2666", # ♦
	Clubs = "\u2663",    # ♣
	Spades = "\u2660",   # ♠
	Joker = "\U0001F0CF" # 🃏
)
# Card values and suits
card_values <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
card_suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
cards <- unlist(lapply(card_suits, function(suit) {
	lapply(card_values, function(value) {
	card(value, suit)
	})
}), recursive = FALSE)

# Create two Joker cards using the card constructor from the deck package
jokers <- list(
  card("Joker", "Joker", display = "Joker🃏 (Red)"),
  card("Joker", "Joker", display = "Joker🃏 (Black)")
)

# Create the French deck with two jokers using the deck class
deck_french <- deck(optional_cards = jokers)

# Export the deck using usethis::use_data
usethis::use_data(deck_french, overwrite = TRUE)

