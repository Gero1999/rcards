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
