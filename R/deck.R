##' Deck constructor
##'
##' @param cards List of main card objects to include (defaults to French deck if NULL)
##' @param optional_cards List of additional card objects to include (e.g., jokers)
##' @param language Language for card display (default: "en")
##' @return An object of class 'deck'
deck <- function(cards = NULL, optional_cards = NULL, language = "en") {
  # Add optional cards if provided
  all_cards <- cards
  if (!is.null(optional_cards) && length(optional_cards) > 0) {
    all_cards <- c(all_cards, optional_cards)
  }
  # Store all_cards as the main value, and other details as attributes
  structure(
    all_cards,
    main_cards = cards,
    optional_cards = optional_cards,
    language = language,
    class = "deck"
  )
}

#' Print method for deck
print.deck <- function(x, ...) {
  n_cards <- length(unclass(x))
  n_optional <- if (is.null(attr(x, "optional_cards"))) 0 else length(attr(x, "optional_cards"))
  lang <- attr(x, "language")
  cat("Deck (", n_cards, " cards, ", n_optional, " optional cards, language: ", lang, ")\n", sep = "")
  invisible(x)
}

#' Shuffle a deck
shuffle.deck <- function(x, ...) {
  n <- length(unclass(x))
  idx <- sample(n)
  shuffled <- unclass(x)[idx]
  # preserve attributes
  attributes(shuffled) <- attributes(x)
  class(shuffled) <- class(x)
  shuffled
}

#' Discard a card at a given index from the deck
discard_card.deck <- function(x, index) {
  cards <- unclass(x)
  if (index < 1 || index > length(cards)) stop("Index out of bounds")
  cards <- cards[-index]
  # preserve attributes
  attr(cards, "main_cards") <- attr(x, "main_cards")
  attr(cards, "optional_cards") <- attr(x, "optional_cards")
  attr(cards, "language") <- attr(x, "language")
  class(cards) <- class(x)
  cards
}
