#' Play a round of Blackjack against the dealer
#' @param players List of one to seven funded player objects.
#' @param wager Positive wager per seat, or one wager recycled to all seats.
#' @param rules Configuration from blackjack_rules().
#' @param seed Optional reproducible shuffle seed, preserving caller RNG.
#' @param pack Optional ordered complete shoe matching the configured decks.
#' @return A blackjack_game inheriting from game. Use play() with one controller
#' per player; the dealer acts automatically. See docs/rules/blackjack.md.
#' @export
blackjack_game <- function(players=list(player("You",points=100)),wager=10,
                           rules=blackjack_rules(),seed=NULL,pack=NULL) {
  new_blackjack(players,wager,rules,seed,pack)
}

#' @export
game_legal_actions.blackjack_game <- function(game) blackjack_legal(game)
#' @export
game_action.blackjack_game <- function(game,decision) {
  decision <- validate_game_decision(decision)
  if(!is.null(decision$amount) || length(decision$cards)) stop("Blackjack actions take no amounts or positions")
  blackjack_step(game,decision$action)
}
#' @export
game_complete.blackjack_game <- function(game) game$complete
#' @export
game_hand_complete.blackjack_game <- function(game) game$complete
#' @export
game_result.blackjack_game <- function(game) {
  list(game_type="blackjack",complete=game$complete,hand_complete=game$complete,
    player_names=vapply(game$players,`[[`,"","name"),stacks=vapply(game$players,`[[`,0,"points"),
    results=if(game$complete) game$results else list())
}
#' @export
game_observation.blackjack_game <- function(game,seat=NULL) {
  seat <- observation_seat(game,seat)
  list(game_type="blackjack",actor=game$actor,seat=seat,legal=game_legal_actions(game),
    player_names=vapply(game$players,`[[`,"","name"),stacks=vapply(game$players,`[[`,0,"points"),
    hands=if(is.null(seat)) list() else game$hands[[seat]],active_hand=game$active_hand,
    hand_counts=lapply(game$hands,function(hands) vapply(hands,function(h) length(h$cards),0L)),
    dealer=if(game$complete) game$dealer else list(game$dealer[[1L]],NULL),
    complete=game$complete,history=game$history)
}

#' Create a baseline Blackjack controller
#' @return A game_controller using only public observations and legal actions.
#' This is a small heuristic, not certified basic strategy or card counting.
#' @export
blackjack_bot <- function() structure(list(),class=c("blackjack_bot","game_controller"))
#' @export
choose_action.blackjack_bot <- function(controller,observation) {
  o <- observation
  if(!identical(o$game_type,"blackjack") || is.null(o$actor)) stop("Expected an active Blackjack observation")
  cards <- o$hands[[o$active_hand]]$cards;total <- blackjack_total(cards)
  legal <- o$legal$actions
  action <- if("split" %in% legal && cards[[1]]$value %in% c("A","8")) "split" else
    if("double" %in% legal && total$total %in% 9:11) "double" else
      if(total$total<17 || (total$soft && total$total<18)) "hit" else "stand"
  list(decision=list(action=action),controller=controller)
}
