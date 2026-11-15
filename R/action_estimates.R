#' Estimate legal action outcomes from a public observation
#' @param observation Public observation for the acting seat, from game_observation().
#' @param n Samples per legal action, one through 10000.
#' @param controllers Optional non-console continuation controllers. Blackjack
#' uses one controller for the acting seat; Scopa uses one per seat.
#' @param seed Optional seed preserving caller RNG.
#' @param max_actions Maximum continuation actions per rollout.
#' @param max_evaluations Bound on n times the number of legal candidate actions.
#' @return An rcards_estimate describing the sampling model and continuation
#' assumptions. Custom controllers are trusted R code and should be stateless
#' or self-contained: external mutable state can invalidate comparisons.
#' @export
estimate_actions <- function(observation,n=100L,controllers=NULL,seed=NULL,
                             max_actions=1000L,max_evaluations=10000L) {
  o <- observation;n <- estimate_n(n,10000L)
  estimate_n(max_actions,1000000L);estimate_n(max_evaluations,1000000L)
  if(!is.list(o) || is.null(o$actor) || is.null(o$seat) || o$actor!=o$seat)
    stop("Provide the acting seat's active public observation")
  switch(o$game_type,
    blackjack=estimate_blackjack_actions(o,n,controllers,seed,max_actions,max_evaluations),
    stop("Action estimation is not available for this game"))
}

check_stat_controllers <- function(controllers,seats) {
  if(!is.list(controllers) || length(controllers)!=seats ||
     !all(vapply(controllers,is_game_controller,logical(1))) ||
     any(vapply(controllers,inherits,logical(1),"console_player")))
    stop("Provide non-console controllers for all simulated seats")
  controllers
}

check_rollout_budget <- function(n,actions,budget) {
  if(!actions || n*actions>budget) stop("Requested action rollouts exceed max_evaluations or have no legal actions")
}

finish_stat_rollout <- function(game,controllers,max_actions) {
  played <- play(game,controllers,max_actions=max_actions)
  if(!played$status %in% c("complete","hand_complete")) stop("Rollout incomplete; increase max_actions or fix continuation controllers")
  played$game
}

blackjack_public_model <- function(o) {
  if(is.null(o$rules)) stop("Observation needs public Blackjack rules")
  rules <- do.call(blackjack_rules,o$rules)
  cards <- rep(as_card_list(poker_pack()),rules$decks)
  # Remove only visible own cards and dealer upcard. Other seats are omitted.
  known <- c(unlist(lapply(o$hands,`[[`,"cards"),recursive=FALSE),o$dealer[1L])
  for(card in known) {
    id <- poker_card_ids(list(card))
    ids <- vapply(cards,function(x) poker_card_ids(list(x)),"")
    index <- match(id,ids)
    if(is.na(index)) stop("Observed cards exceed configured shoe composition")
    cards <- cards[-index]
  }
  up <- o$dealer[[1L]]$value
  ranks <- vapply(cards,`[[`,"","value")
  valid <- which(if(up=="A") !ranks %in% c("10","J","Q","K") else
    if(up %in% c("10","J","Q","K")) ranks!="A" else rep(TRUE,length(cards)))
  if(!length(valid)) stop("No dealer hole card is consistent with the completed natural check")
  hole_index <- valid[sample.int(length(valid),1L)]
  hole <- cards[[hole_index]];cards <- cards[-hole_index]
  history <- Filter(function(event) event$player==o$actor,o$history)
  history <- lapply(history,function(event) {event$player<-1L;event})
  structure(list(players=list(player(o$player_names[o$actor],points=o$stacks[o$actor])),
    hands=list(o$hands),dealer=list(o$dealer[[1L]],hole),shoe=cards[sample.int(length(cards))],
    rules=rules,actor=1L,active_hand=o$active_hand,history=history,complete=FALSE,results=list()),
    class=c("blackjack_game","game"))
}

estimate_blackjack_actions <- function(o,n,controllers,seed,max_actions,budget) {
  if(is.null(controllers)) controllers <- list(blackjack_bot())
  controllers <- check_stat_controllers(controllers,1L)
  actions <- o$legal$actions
  check_rollout_budget(n,length(actions),budget)
  gains <- matrix(NA_real_,n,length(actions),dimnames=list(NULL,actions))
  invested <- sum(vapply(o$hands,`[[`,0,"bet"))
  baseline <- o$stacks[o$actor]+invested
  with_poker_seed(seed,function() {
    for(i in seq_len(n)) {
      model <- blackjack_public_model(o)
      for(j in seq_along(actions)) {
        game <- blackjack_step(model,actions[j])
        game <- finish_stat_rollout(game,controllers,max_actions)
        gains[i,j] <<- game$players[[1]]$points-baseline
      }
    }
  })
  estimates <- do.call(rbind,lapply(seq_along(actions),function(j) {
    chance <- estimate_summary(as.numeric(gains[,j]>0),binary=TRUE)
    cbind(action=actions[j],metric="net_chip_profit",estimate_summary(gains[,j]),
      profit_probability=chance$estimate,profit_lower=chance$lower,profit_upper=chance$upper)
  }))
  stat_report("blackjack_action_profit","monte_carlo",
    c("Single-seat continuation: other players and their future decisions are omitted.",
      "The shoe is resampled from configured decks minus visible own cards and dealer upcard.",
      "Dealer hole cards are conditioned on the engine's completed natural check.",
      "All existing own hands are settled; profit includes their initial wagers and any extra split/double stakes.",
      "Continuation controllers are reset for each action/trial; common hidden draws pair comparisons."),
    estimates,list(evaluations=n*length(actions)))
}
