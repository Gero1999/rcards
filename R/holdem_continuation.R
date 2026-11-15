#' Deal another Hold'em hand with settled stacks
#'
#' Retains fixed seat indices, player metadata, and blind amounts. The dealer
#' moves to the next funded seat clockwise; zero-stack seats receive no cards
#' and take no turns. At least two funded seats must remain. The hand is dealt
#' from a fresh shuffled pack unless an ordered pack is supplied.
#' @param game A completed holdem_hand.
#' @param seed Optional shuffle seed, preserving caller RNG state.
#' @param pack Optional complete ordered French pack, without jokers.
#' @return A new holdem_hand with updated hand_number and the same table seats.
#' This advances explicitly; game_complete() continues to describe one hand.
#' @export
holdem_next_hand <- function(game, seed = NULL, pack = NULL) {
  if (!inherits(game, "holdem_hand")) stop("Expected a holdem_hand")
  if (!isTRUE(game$complete)) stop("Finish the current hand before dealing again")
  funded <- which(holdem_stacks(game) > 0)
  if (length(funded) < 2L) stop("At least two funded seats are needed for another hand")
  order <- clockwise_seats(length(game$players), game$dealer)
  dealer <- order[order %in% funded][1L]
  hand_number <- if (is.null(game$hand_number)) 1L else game$hand_number
  new_holdem_hand(game$players, dealer, game$small_blind, game$big_blind,
    seed, pack, allow_eliminated = TRUE, hand_number = hand_number + 1L)
}

#' @export
game_next_hand.holdem_hand <- function(game) holdem_next_hand(game)
