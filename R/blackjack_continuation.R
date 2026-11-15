#' Continue Blackjack with settled bankrolls
#'
#' Starts a fresh round at the same seats, retaining player metadata and rules.
#' Seats unable to cover their chosen wager sit out with empty hands; at least
#' one seat must be funded. No bankroll is replenished automatically. Each round
#' uses a fresh complete shoe, rather than the preceding round's remaining cards.
#' @param game A completed blackjack_game.
#' @param wager Positive scalar wager or one positive wager per original seat.
#' NULL retains the preceding round's base wagers, excluding doubles and splits.
#' @param seed Optional reproducible shuffle seed, preserving caller RNG.
#' @param pack Optional ordered complete shoe matching the configured decks.
#' @return A new blackjack_game with settled bankrolls carried forward, stable
#' seat indices, and an incremented round_number. The input is unchanged.
#' @export
blackjack_next_round <- function(game,wager=NULL,seed=NULL,pack=NULL) {
  if(!inherits(game,"blackjack_game")) stop("Expected a blackjack_game")
  if(!isTRUE(game$complete)) stop("Finish the current Blackjack round first")
  n <- length(game$players)
  if(is.null(wager)) wager <- blackjack_round_wagers(game)
  if(length(wager)==1L) wager <- rep(wager,n)
  if(!is.numeric(wager) || length(wager)!=n || anyNA(wager) ||
     any(!is.finite(wager)) || any(wager<=0)) stop("Provide positive wagers for all players")
  bankrolls <- vapply(game$players,`[[`,0,"points")
  for(bankroll in bankrolls) check_number(bankroll,"stack",0)
  participating <- which(bankrolls>=wager)
  if(!length(participating)) stop("No player can cover the next Blackjack wager")

  # Deal only participating seats, then restore their original indices. The
  # regular engine already skips seats whose hand list is empty.
  next_round <- new_blackjack(game$players[participating],wager[participating],
    game$rules,seed,pack)
  players <- game$players
  for(i in seq_len(n)) players[[i]]$hand <- list()
  players[participating] <- next_round$players
  hands <- rep(list(list()),n)
  hands[participating] <- next_round$hands
  if(!is.null(next_round$actor)) next_round$actor <- participating[next_round$actor]
  next_round$results <- lapply(next_round$results,function(result) {
    result$player <- participating[result$player]
    result
  })
  next_round$history <- lapply(next_round$history,function(event) {
    event$player <- participating[event$player]
    event
  })
  next_round$players <- players
  next_round$hands <- hands
  next_round$initial_stacks <- bankrolls
  next_round$wagers <- wager
  next_round$round_number <- if(is.null(game$round_number)) 2L else game$round_number+1L
  next_round
}

blackjack_round_wagers <- function(game) {
  if(is.null(game$wagers)) stop("Supply wagers explicitly for a round without recorded base wagers")
  game$wagers
}

#' @export
game_next_hand.blackjack_game <- function(game) blackjack_next_round(game)
