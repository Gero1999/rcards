#' Start another match with the same players and rules
#'
#' Explicitly starts a new Mus or Scopa match after the score target has been
#' reached, or another Mentiroso deal after its winner is confirmed. Player
#' objects and seat order are retained; new cards replace their old hands.
#' Mus and Scopa match scores reset to zero. Dealer or mano moves one seat
#' clockwise from the completed match. No previous private state is carried.
#'
#' Use game_next_hand() instead to continue an unfinished Mus or Scopa match
#' while keeping its scores. Controllers are separate from game objects and
#' can be reused by the caller in either case.
#' @param game A completed mus_game, scopa_game, or liar_game.
#' @param seed Optional reproducible shuffle seed, preserving caller RNG.
#' @return A fresh game of the same type, with the same players and rules.
#' @export
game_rematch <- function(game,seed=NULL) UseMethod("game_rematch")

#' @export
game_rematch.default <- function(game,seed=NULL) {
  stop("This game does not support a rematch")
}

#' @export
game_rematch.mus_game <- function(game,seed=NULL) {
  if(!isTRUE(game_complete(game))) stop("Finish the current Mus match before starting a rematch")
  mus_game(game$players,game$rules,mano=game$mano %% 4L+1L,seed=seed)
}

#' @export
game_rematch.scopa_game <- function(game,seed=NULL) {
  if(!isTRUE(game_complete(game))) stop("Finish the current Scopa match before starting a rematch")
  dealer <- game$dealer %% length(game$players)+1L
  scopa_game(game$players,game$rules,dealer=dealer,seed=seed)
}

#' @export
game_rematch.liar_game <- function(game,seed=NULL) liar_next_deal(game,seed=seed)

#' Deal Mentiroso again with the same players and rules
#'
#' Starts a new deal only after the preceding deal has a confirmed winner.
#' Dealer rotates clockwise, all cards are redealt, and the pile, claim,
#' challenge reveal, and history are cleared. The game still awards one winner
#' per deal; no match points or elimination rules are introduced.
#' @param game A completed liar_game.
#' @param seed Optional reproducible shuffle seed, preserving caller RNG.
#' @param pack Optional ordered complete configured pack, without jokers.
#' @return A fresh liar_game with the same player objects, seat order and rules,
#' and an incremented deal_number (the original deal counts as one).
#' @export
liar_next_deal <- function(game,seed=NULL,pack=NULL) {
  if(!inherits(game,"liar_game") || !isTRUE(game_complete(game)))
    stop("Finish the current Mentiroso deal before dealing again")
  dealer <- game$dealer %% length(game$players)+1L
  next_game <- liar_game(game$players,game$rules,dealer=dealer,seed=seed,pack=pack)
  next_game$deal_number <- if(is.null(game$deal_number)) 2L else game$deal_number+1L
  next_game
}

#' @export
game_next_hand.liar_game <- function(game) liar_next_deal(game)
