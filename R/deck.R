##' Deck constructor
##'
##' @param cards List of main card objects to include (French cards if NULL)
##' @param optional_cards List of additional card objects to include (e.g., jokers)
##' @param language Language for card display (default: "en")
##' @param include_optional Include optional cards (FALSE by default).
#' @return An object of class 'deck'
#' @export
deck <- function(cards = NULL, optional_cards = NULL, language = "en", include_optional = FALSE) {
  if (!is.logical(include_optional) || length(include_optional) != 1L ||
      is.na(include_optional)) stop("include_optional must be TRUE or FALSE")
  if (is.null(cards)) {
    cards <- unlist(lapply(c("Hearts", "Diamonds", "Clubs", "Spades"), function(suit) {
      lapply(c("A", as.character(2:10), "J", "Q", "K"), card, suit = suit)
    }), recursive = FALSE)
    if (is.null(optional_cards)) {
      optional_cards <- list(card("Joker", "Joker", "Red Joker"),
                             card("Joker", "Joker", "Black Joker"))
    }
  }
  if (!include_optional) optional_cards <- list()
  if (!is.list(cards) || !all(vapply(cards, inherits, logical(1), "card"))) {
    stop("cards must be a list of card objects")
  }
  if (!is.null(optional_cards) && (!is.list(optional_cards) ||
      !all(vapply(optional_cards, inherits, logical(1), "card")))) {
    stop("optional_cards must be a list of card objects")
  }
  if (!is.character(language) || length(language) != 1L || is.na(language) ||
      !nzchar(language)) stop("language must be a nonempty character scalar")
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
#' @param x A deck object.
#' @param ... Unused arguments.
#' @export
print.deck <- function(x, ...) {
  n_cards <- length(unclass(x))
  n_optional <- if (is.null(attr(x, "optional_cards"))) 0 else length(attr(x, "optional_cards"))
  lang <- attr(x, "language")
  cat("Deck (", n_cards, " cards, ", n_optional, " optional cards, language: ", lang, ")\n", sep = "")
  invisible(x)
}

#' Shuffle a deck
#' @param x A deck object.
#' @param ... Unused arguments.
#' @return A shuffled deck with preserved metadata.
#' @export
shuffle.deck <- function(x, ...) {
  x[sample.int(length(x))]
}

#' Discard cards at specified positions
#' @param x A deck object.
#' @param index Unique integer positions to remove; integer(0) removes nothing.
#' @return A deck without the selected cards.
#' @export
discard_card.deck <- function(x, index) {
  validate_positions(index, length(x))
  x[setdiff(seq_along(x), index)]
}

#' Shuffle a deck
#' @param x A deck object.
#' @param ... Additional method arguments.
#' @return A shuffled deck.
#' @export
shuffle <- function(x, ...) UseMethod("shuffle")

#' Discard cards by position
#' @param x A deck object.
#' @param index Card positions to remove.
#' @return A deck without the selected cards.
#' @export
discard_card <- function(x, index) UseMethod("discard_card")

#' Subset a deck while preserving metadata
#' @param x A deck object.
#' @param i Card positions to retain.
#' @param ... Unused arguments.
#' @param drop Unused; always returns a deck.
#' @return A deck containing the selected cards.
#' @export
`[.deck` <- function(x, i, ..., drop = FALSE) {
  cards <- as_card_list(x)
  selected <- if (missing(i)) cards else cards[i]
  if (any(vapply(selected, is.null, logical(1)))) stop("Index out of bounds")
  rebuild_deck(selected, attr(x, "optional_cards"), attr(x, "language"))
}

as_card_list <- function(x) {
  attributes(x) <- NULL
  x
}

rebuild_deck <- function(cards, optional_cards, language) {
  is_optional <- vapply(cards, function(item) {
    any(vapply(optional_cards, identical, logical(1), item))
  }, logical(1))
  structure(cards, main_cards = cards[!is_optional],
            optional_cards = cards[is_optional], language = language, class = "deck")
}

validate_positions <- function(index, n) {
  if (!is.numeric(index) || anyNA(index) || any(!is.finite(index)) ||
      any(index != floor(index)) || any(index < 1 | index > n) ||
      anyDuplicated(index)) stop("Index out of bounds or invalid positions")
  invisible(index)
}
