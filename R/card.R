suit_symbols <- c(
  Hearts = "\u2665", Diamonds = "\u2666", Clubs = "\u2663",
  Spades = "\u2660", Joker = "\U0001F0CF", Oros = "\U0001F4B0",
  Copas = "\U0001F377", Espadas = "\U0001F5E1", Bastos = "\U0001F528"
)

#' Create a card
#' @param value Card rank or value.
#' @param suit Suit name.
#' @param display Optional custom display text.
#' @return A card object.
#' @export
# Define the 'card' S3 class and constructor with Unicode display
card <- function(value, suit, display = NULL) {
	if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) stop("value must be a nonempty character scalar")
  if (!is.character(suit) || length(suit) != 1L || is.na(suit) ||
      !nzchar(suit)) stop("suit must be a nonempty character scalar")
  if (!is.null(display) && (!is.character(display) || length(display) != 1L ||
      is.na(display))) stop("display must be a character scalar")
  symbol <- unname(suit_symbols[suit])
  if (is.na(symbol)) symbol <- ""
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
