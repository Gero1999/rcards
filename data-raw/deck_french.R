# Unicode symbols for suits
suit_symbols <- c(
	Hearts = "\u2665",   # ♥
	Diamonds = "\u2666", # ♦
	Clubs = "\u2663",    # ♣
	Spades = "\u2660",   # ♠
	Joker = "\U0001F0CF" # 🃏
)

# Define the 'card' S3 class and constructor with Unicode display
card <- function(value, suit, display = NULL) {
	symbol <- suit_symbols[suit]
	if (is.null(display)) {
		display <- paste0(value, symbol)
	}
	structure(
		list(
			value = value,   # e.g., "A", "2", ..., "K"
			suit = suit,     # e.g., "Hearts", "Diamonds", "Clubs", "Spades"
			symbol = symbol, # Unicode symbol for the suit
			display = display # e.g., "A♠"
		),
		class = "card"
	)
}

# Print method for 'card' objects
print.card <- function(x, ...) {
	cat(x$display, "\n")
}

# Define values and suits for a French deck
card_values <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
card_suits <- c("Hearts", "Diamonds", "Clubs", "Spades")



# Create the standard 52 cards
deck_french <- lapply(card_suits, function(suit) {
	lapply(card_values, function(value) {
		card(value, suit)
	})
})
deck_french <- unlist(deck_french, recursive = FALSE)

# Add two Jokers (Red and Black)
jokers <- list(
	card("Joker", "Joker", display = "Joker🃏 (Red)"),
	card("Joker", "Joker", display = "Joker🃏 (Black)")
)
deck_french <- c(deck_french, jokers)

# Export the deck using usethis::use_data
usethis::use_data(deck_french, overwrite = TRUE)
