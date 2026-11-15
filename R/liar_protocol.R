#' Play Mentiroso (Liar) with face-down rank claims
#' @param players Two through eight distinct player names or player objects.
#' @param rules Configuration from liar_rules(); Spanish pack by default.
#' @param dealer Dealer seat; the following seat leads.
#' @param seed Optional seed preserving caller RNG.
#' @param pack Optional ordered complete configured pack, without jokers.
#' @details A claim uses game_action(game, list(action="claim", amount="7",
#' cards=c(1,3))). amount is the declared rank, not a wager; cards are positions
#' in the acting hand. Opponents then use action="challenge" or action="pass".
#' The rank stays fixed until a challenge, after which the pickup player leads.
#' Last-card claims must survive the response window before winning.
#' @return A liar_game inheriting from game, playable through play(). Public
#' observations never disclose unchallenged batches or opponents' hands.
#' @export
liar_game <- function(players=c("You","Bot 1","Bot 2"),rules=liar_rules(),
                      dealer=length(players),seed=NULL,pack=NULL) {
  new_liar(players,rules,dealer,seed,pack)
}
#' @export
game_legal_actions.liar_game <- function(game) liar_legal(game)
#' @export
game_action.liar_game <- function(game,decision) {
  decision <- validate_game_decision(decision)
  cards <- if(is.null(decision$cards)) integer() else decision$cards
  liar_step(game,decision$action,cards,decision$amount)
}
#' @export
game_complete.liar_game <- function(game) game$complete
#' @export
game_hand_complete.liar_game <- function(game) game$complete
#' @export
game_result.liar_game <- function(game) {
  list(game_type="liar",complete=game$complete,hand_complete=game$complete,
    player_names=vapply(game$players,`[[`,"","name"),
    hand_counts=vapply(game$players,function(p) length(p$hand),0L),winner=game$winner)
}
#' @export
game_observation.liar_game <- function(game,seat=NULL) {
  seat <- observation_seat(game,seat)
  list(game_type="liar",actor=game$actor,seat=seat,phase=game$phase,
    player_names=vapply(game$players,`[[`,"","name"),
    hole=if(is.null(seat)) list() else game$players[[seat]]$hand,
    hand_counts=vapply(game$players,function(p) length(p$hand),0L),
    pile_count=length(game$pile),rank=game$rank,
    claim=if(is.null(game$last_claim)) NULL else game$last_claim[c("player","rank","count")],
    responders=game$responders,reveal=game$last_reveal,history=game$history,
    deal_number=if(is.null(game$deal_number)) 1L else game$deal_number,
    rules=game$rules,legal=liar_legal(game),complete=game$complete,winner=game$winner)
}
