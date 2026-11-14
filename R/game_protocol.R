#' Shared interface for playable games
#'
#' Hold'em and Mus inherit from game and implement this protocol. The plain
#' game() constructor remains a general container and cannot be played without
#' game-specific methods. Rules stay inside the individual engines.
#'
#' game_complete() means the entire game is over: one Hold'em hand, or a Mus
#' game at its target score. game_hand_complete() also marks an intermediate Mus
#' hand boundary. game_next_hand() advances Mus at such a boundary.
#'
#' Observations and results exclude opponents' hidden cards and deck order.
#' These projections protect controller inputs, not access to the full in-process
#' R state. Code holding the game object can inspect its private fields directly.
#' @param game A playable game object.
#' @param seat Seat whose own cards may be included; defaults to the actor.
#' At completion, NULL produces a public observation with no private cards.
#' @param decision A named list with action and optional amount or cards.
#' @return game_observation() returns a seat-specific view; game_legal_actions()
#' returns legal actions for the current actor; game_action() returns updated
#' game state; completion methods return logical scalars; game_result() returns
#' a public summary; game_next_hand() returns a new hand preserving game scores.
#' @export
game_observation <- function(game, seat = NULL) UseMethod("game_observation")

#' @rdname game_observation
#' @export
game_legal_actions <- function(game) UseMethod("game_legal_actions")

#' @rdname game_observation
#' @export
game_action <- function(game, decision) UseMethod("game_action")

#' @rdname game_observation
#' @export
game_complete <- function(game) UseMethod("game_complete")

#' @rdname game_observation
#' @export
game_hand_complete <- function(game) UseMethod("game_hand_complete")

#' @rdname game_observation
#' @export
game_result <- function(game) UseMethod("game_result")

#' @rdname game_observation
#' @export
game_next_hand <- function(game) UseMethod("game_next_hand")

#' @export
game_observation.game <- function(game, seat = NULL) stop("This game has no observation method")
#' @export
game_legal_actions.game <- function(game) stop("This game has no legal-action method")
#' @export
game_action.game <- function(game, decision) stop("This game has no action method")
#' @export
game_complete.game <- function(game) stop("This game has no completion method")
#' @export
game_hand_complete.game <- function(game) stop("This game has no hand-completion method")
#' @export
game_result.game <- function(game) stop("This game has no results method")
#' @export
game_next_hand.game <- function(game) stop("This game cannot advance to another hand")

validate_game_decision <- function(decision) {
  allowed <- c("action", "amount", "cards")
  if (!is.list(decision) || is.null(names(decision)) || anyNA(names(decision)) ||
      any(!names(decision) %in% allowed) || anyDuplicated(names(decision)) ||
      !is.character(decision$action) || length(decision$action) != 1L ||
      is.na(decision$action) || !nzchar(decision$action)) {
    stop("Decision must contain one action and optional amount/cards fields")
  }
  decision
}

observation_seat <- function(game, seat) {
  if (is.null(seat)) seat <- game$actor
  if (!is.null(seat)) check_number(seat, "seat", 1, length(game$players), TRUE)
  seat
}

#' @export
game_observation.holdem_hand <- function(game, seat = NULL) {
  seat <- observation_seat(game, seat)
  if (!game$complete) observation <- holdem_observation(game) else {
    observation <- list(actor = NULL, board = game$board,
      stacks = holdem_stacks(game), contributions = game$contributions,
      round_bets = game$round_bets, folded = game$folded, street = game$street,
      big_blind = game$big_blind, pot = sum(game$contributions),
      legal = holdem_legal_actions(game), history = game$history)
  }
  observation$hole <- if (is.null(seat)) list() else game$players[[seat]]$hand
  observation$seat <- seat
  observation$player_names <- vapply(game$players, `[[`, "", "name")
  observation$game_type <- "holdem"
  observation$complete <- game$complete
  observation
}

#' @export
game_legal_actions.holdem_hand <- function(game) holdem_legal_actions(game)
#' @export
game_action.holdem_hand <- function(game, decision) {
  decision <- validate_game_decision(decision)
  if (length(decision$cards)) stop("Hold'em actions do not take discard positions")
  holdem_action(game, decision$action, decision$amount)
}
#' @export
game_complete.holdem_hand <- function(game) game$complete
#' @export
game_hand_complete.holdem_hand <- function(game) game$complete
#' @export
game_result.holdem_hand <- function(game) {
  list(game_type = "holdem", complete = game$complete, hand_complete = game$complete,
    phase = game$street, player_names = vapply(game$players, `[[`, "", "name"),
    stacks = holdem_stacks(game), board = game$board,
    payouts = if (game$complete) game$payouts else NULL,
    pot = if (game$complete) 0 else sum(game$contributions))
}

#' Observe a Mus seat without disclosing other hands
#'
#' Only the requested seat's own cards are included. Discard history records
#' counts rather than card positions. Pair/juego eligibility is included only
#' when publicly declared by the current lance.
#' @param game A mus_game state.
#' @param seat Seat to observe; defaults to the actor, or public-only at completion.
#' @return A public game view plus the selected seat's own hand.
#' @export
mus_observation <- function(game, seat = NULL) {
  if (!inherits(game, "mus_game")) stop("Expected mus_game")
  seat <- observation_seat(game, seat)
  history <- lapply(game$history, function(event) {
    public <- event[intersect(names(event), c("hand", "phase", "player", "action", "amount"))]
    if (identical(event$action, "discard")) public$discard_count <- length(event$cards)
    public
  })
  list(game_type = "mus", actor = game$actor, seat = seat,
    hole = if (is.null(seat)) list() else game$players[[seat]]$hand,
    player_names = vapply(game$players, `[[`, "", "name"),
    teams = mus_team(1:4), scores = game$scores, rules = game$rules,
    mano = game$mano, hand_number = game$hand_number, phase = game$phase,
    eligible = if (game$phase %in% c("pares", "juego", "punto")) game$eligible else NULL,
    wager = if (game$phase %in% c("mus", "exchange")) 0 else game$wager,
    ordago = isTRUE(game$ordago) && !game$hand_complete,
    legal = game_legal_actions(game), history = history, results = game$results,
    complete = game$complete, hand_complete = game$hand_complete)
}

#' @export
game_observation.mus_game <- function(game, seat = NULL) mus_observation(game, seat)
#' @export
game_legal_actions.mus_game <- function(game) {
  legal <- mus_legal_actions(game)
  if (any(c("bet", "raise") %in% legal$actions)) {
    legal$min_stake <- if ("bet" %in% legal$actions) 2L else game$wager + 2L
  }
  if ("discard" %in% legal$actions) legal$card_positions <- 1:4
  legal
}
#' @export
game_action.mus_game <- function(game, decision) {
  decision <- validate_game_decision(decision)
  cards <- if (is.null(decision$cards)) integer() else decision$cards
  mus_action(game, decision$action, decision$amount, cards)
}
#' @export
game_complete.mus_game <- function(game) game$complete
#' @export
game_hand_complete.mus_game <- function(game) game$hand_complete
#' @export
game_result.mus_game <- function(game) {
  list(game_type = "mus", complete = game$complete, hand_complete = game$hand_complete,
    phase = game$phase, player_names = vapply(game$players, `[[`, "", "name"),
    scores = game$scores, teams = mus_team(1:4), hand_number = game$hand_number,
    winner = if (game$complete) game$winner else NULL, results = game$results)
}
#' @export
game_next_hand.mus_game <- function(game) mus_next_hand(game)
