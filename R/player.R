#' Player constructor
#'
#' @param hand List of cards or deck object (default: empty list)
#' @param role Player role (default: "standard")
#' @param points Player points (default: 0)
#' @return An object of class 'player'
player <- function(name = "Player 1", hand = list(), role = "standard", points = 0) {
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
print.player <- function(x, ...) {
	cat("Player (role:", x$role, ", points:", x$points, ", hand size:", length(x$hand), ")\n")
	invisible(x)
}

#' Take deck cards into player's hand
#' @param player A player object
#' @param deck A deck object
#' @param n Number of cards to take (default: 1)
#' @return Updated player object with new hand and updated deck
#' @export
take_cards <- function(player, deck, n = 1) {
  drawn_ix <- sample(length(deck$cards), n)
  drawn_cards <- deck[drawn_ix]
  remaining_deck <- deck[setdiff(seq_along(deck$cards), drawn_ix)]
  player$hand <- c(player$hand, drawn_cards)
  list(player = player, deck = remaining_deck)
}

#' Leave cards from player's hand to deck
#' @param player A player object
#' @param deck A deck object
#' @param card_indices Indices of cards in player's hand to leave
#' @return Updated player object with new hand and updated deck
#' @export
leave_cards <- function(player, deck, card_indices) {
    leaving_cards <- player$hand[card_indices]
    player$hand <- player$hand[-card_indices]
    deck <- c(deck, leaving_cards)
    list(player = player, deck = deck)
}
