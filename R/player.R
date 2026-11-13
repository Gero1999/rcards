#' Player constructor
#'
#' @param name Player name.
#' @param hand List of cards or deck object (default: empty list)
#' @param role Player role (default: "standard")
#' @param points Player points (default: 0)
#' @return An object of class 'player'
#' @export
player <- function(name = "Player 1", hand = list(), role = "standard", points = 0) {
    if (!inherits(hand, "deck")) hand <- deck(cards = hand)
	structure(
		list(
            name = name,
			hand = hand,
			role = role,
			points = points
		),
		class = "player"
	)
}

#' Print method for player
#' @param x A player object.
#' @param ... Unused arguments.
#' @export
print.player <- function(x, ...) {
	cat("Player (role:", x$role, ", points:", x$points, ", hand size:", length(x$hand), ")\n")
	invisible(x)
}

#' Take deck cards into player's hand
#' @param player A player object
#' @param deck A deck object
#' @param n Number of cards to take from the top (default: 1). Shuffle first for random draws.
#' @return Updated player object with new hand and updated deck
#' @export
take_cards <- function(player, deck, n = 1) {
  if (!inherits(player, "player") || !inherits(deck, "deck")) {
    stop("Expected a player and a deck")
  }
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
      n != floor(n) || n < 0 || n > length(deck)) stop("Invalid number of cards")
  positions <- seq_len(n)
  drawn <- deck[positions]
  hand <- player$hand
  player$hand <- rebuild_deck(c(as_card_list(hand), as_card_list(drawn)),
    c(attr(hand, "optional_cards"), attr(drawn, "optional_cards")), attr(hand, "language"))
  list(player = player, deck = discard_card(deck, positions))
}

#' Leave cards from player's hand to deck
#' @param player A player object
#' @param deck A deck object
#' @param card_indices Unique positions in the hand to return to the bottom of the deck.
#' @return Updated player object with new hand and updated deck
#' @export
leave_cards <- function(player, deck, card_indices) {
  if (!inherits(player, "player") || !inherits(deck, "deck")) {
    stop("Expected a player and a deck")
  }
  validate_positions(card_indices, length(player$hand))
  leaving <- player$hand[card_indices]
  player$hand <- discard_card(player$hand, card_indices)
  returned <- rebuild_deck(c(as_card_list(deck), as_card_list(leaving)),
    c(attr(deck, "optional_cards"), attr(leaving, "optional_cards")), attr(deck, "language"))
  list(player = player, deck = returned)
}
