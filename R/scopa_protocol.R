#' Start a two-, three-, or four-player Scopa match
#' @param players Distinct player names or player objects. Four seats form
#' partnerships 1/3 and 2/4; two or three seats play individually.
#' @param rules Configuration from scopa_rules().
#' @param dealer Dealer seat; the following seat acts first.
#' @param seed Optional reproducible shuffle seed, preserving caller RNG.
#' @param pack Optional ordered complete forty-card Italian pack.
#' @details Programmatic moves use game_action(game, list(action="play", cards=2,
#' amount=1)): cards is a hand position, amount is its capture option number.
#' game_legal_actions(game)$captures lists table positions for each choice.
#' Omit amount only when that hand card has a single legal option.
#' @return A scopa_game inheriting from game. Use play() for one deal or
#' play(..., until="game") for a match. See docs/rules/scopa.md.
#' @export
scopa_game <- function(players=c("You","Bot"),rules=scopa_rules(),dealer=length(players),seed=NULL,pack=NULL) {
  new_scopa(players,rules,dealer,seed,pack)
}
#' @export
game_legal_actions.scopa_game <- function(game) scopa_legal(game)
#' @export
game_action.scopa_game <- function(game,decision) {
  decision <- validate_game_decision(decision)
  if(!identical(decision$action,"play")) stop("Illegal Scopa action")
  scopa_step(game,decision$cards,decision$amount)
}
#' @export
game_complete.scopa_game <- function(game) game$complete
#' @export
game_hand_complete.scopa_game <- function(game) game$hand_complete
#' @export
game_next_hand.scopa_game <- function(game) scopa_next_deal(game)
#' @export
game_result.scopa_game <- function(game) {
  list(game_type="scopa",complete=game$complete,hand_complete=game$hand_complete,
    player_names=vapply(game$players,`[[`,"","name"),teams=game$teams,scores=game$scores,
    hand_number=game$hand_number,winner=game$winner,results=game$results)
}
#' @export
game_observation.scopa_game <- function(game,seat=NULL) {
  seat <- observation_seat(game,seat)
  list(game_type="scopa",actor=game$actor,seat=seat,legal=scopa_legal(game),
    player_names=vapply(game$players,`[[`,"","name"),teams=game$teams,scores=game$scores,
    hand_number=game$hand_number,dealer=game$dealer,
    hole=if(is.null(seat)) list() else game$players[[seat]]$hand,
    table_cards=game$table_cards,stock_count=length(game$stock),
    hand_counts=vapply(game$players,function(p) length(p$hand),0L),
    capture_counts=vapply(game$captured,length,0L),sweeps=game$sweeps,
    history=game$history,complete=game$complete,hand_complete=game$hand_complete)
}

#' Create a public-information Scopa controller
#' @return A game_controller using legal captures, table cards and own hand.
#' It favors captures, coins, settebello and sweeps; this is a baseline heuristic.
#' @export
scopa_bot <- function() structure(list(),class=c("scopa_bot","game_controller"))
#' @export
choose_action.scopa_bot <- function(controller,observation) {
  o <- observation
  if(!identical(o$game_type,"scopa") || is.null(o$actor)) stop("Expected an active Scopa observation")
  best <- -Inf;decision <- NULL
  for(i in seq_along(o$legal$captures)) for(j in seq_along(o$legal$captures[[i]])) {
    take <- o$legal$captures[[i]][[j]]
    if(length(take)) {
      cards <- c(list(o$hole[[i]]),o$table_cards[take])
      score <- length(cards)+sum(vapply(cards,function(x) x$suit=="Denari",logical(1)))+
        4*any(vapply(cards,function(x) x$value=="7" && x$suit=="Denari",logical(1)))+
        3*(length(take)==length(o$table_cards))
    } else score <- -as.numeric(o$hole[[i]]$value)/100
    if(score>best) {best<-score;decision<-list(action="play",cards=i,amount=j)}
  }
  list(decision=decision,controller=controller)
}
