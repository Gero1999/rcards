#' Simulate independent games with explicit controller strategies
#' @param game_factory Function of iteration i returning a fresh game. Let
#' constructors draw from the caller RNG (seed=NULL); do not hard-code the same
#' seed unless deliberately studying a fixed scenario. Previously played states
#' are rejected. Constructors that immediately settle naturals/all-ins are valid.
#' @param controllers Non-console controller list, or function(game, i) returning
#' such a list. Controller state resets each run; external mutable closures and
#' correlated factories can violate the independence assumed by intervals.
#' @param n Requested runs, one through 10000.
#' @param until Complete a hand/deal or a whole game/match.
#' @param seed Optional reproducible seed preserving caller RNG.
#' @param max_actions Maximum accepted actions per run.
#' @param on_incomplete error aborts; record excludes unfinished runs from all
#' statistical denominators and records their status. Exclusion can bias results.
#' @param keep_games Retain completed full game objects (including hidden state).
#' @return A list of per-side summaries, per-run values/outcomes, completion
#' statuses and definitions. Poker/Blackjack win/tie/loss means positive/zero/
#' negative net chips, not pot winners or showdown ties. Mus/Scopa hand outcomes
#' compare points gained during the deal; whole-game outcomes use the match winner.
#' @export
simulate_games <- function(game_factory,controllers,n=100L,until=c("hand","game"),
                           seed=NULL,max_actions=10000L,on_incomplete=c("error","record"),keep_games=FALSE) {
  if(!is.function(game_factory)) stop("game_factory must be a function of iteration")
  n <- estimate_n(n,10000L);until<-match.arg(until);on_incomplete<-match.arg(on_incomplete)
  estimate_n(max_actions,1000000L)
  if(!is.logical(keep_games) || length(keep_games)!=1L || is.na(keep_games)) stop("keep_games must be TRUE or FALSE")
  rows <- list();statuses <- list();games <- list();layout <- NULL
  with_poker_seed(seed,function() {
    for(i in seq_len(n)) {
      g <- game_factory(i)
      setup <- simulation_setup(g)
      signature <- list(type=setup$type,labels=setup$labels)
      if(is.null(layout)) layout <<- signature else if(!identical(signature,layout)) stop("Game type and seat/side labels must stay consistent across runs")
      if(length(g$history)) stop("game_factory must return a fresh unplayed game")
      policies <- if(is.function(controllers)) controllers(g,i) else controllers
      policies <- check_stat_controllers(policies,length(g$players))
      played <- play(g,policies,until=until,max_actions=max_actions)
      completed <- if(until=="game") isTRUE(game_complete(played$game)) else isTRUE(game_hand_complete(played$game))
      statuses[[i]] <<- data.frame(run=i,status=played$status,completed=completed)
      if(!completed) {
        if(on_incomplete=="error") stop("Simulation incomplete; increase max_actions or change controllers")
        next
      }
      values <- simulation_outcomes(played$game,setup,until)
      values$run <- i
      rows[[length(rows)+1L]] <<- values
      if(keep_games) games[[length(games)+1L]] <<- played$game
    }
  })
  runs <- if(length(rows)) do.call(rbind,rows) else data.frame()
  summaries <- list()
  if(nrow(runs)) for(side in unique(runs$side)) {
    data <- runs[runs$side==side,,drop=FALSE]
    chance <- estimate_summary(as.numeric(data$outcome=="win"),binary=TRUE)
    ties <- estimate_summary(as.numeric(data$outcome=="tie"),binary=TRUE)
    losses <- estimate_summary(as.numeric(data$outcome=="loss"),binary=TRUE)
    summaries[[length(summaries)+1L]] <- cbind(side=side,label=data$label[1],
      estimate_summary(data$value),win_probability=chance$estimate,win_lower=chance$lower,win_upper=chance$upper,
      tie_probability=ties$estimate,tie_lower=ties$lower,tie_upper=ties$upper,
      loss_probability=losses$estimate,loss_lower=losses$lower,loss_upper=losses$upper)
  }
  statuses <- do.call(rbind,statuses)
  list(type=layout$type,scope=until,requested=n,completed=sum(statuses$completed),
    incomplete=sum(!statuses$completed),confidence_level=0.95,
    definition=if(layout$type %in% c("holdem","blackjack"))
      "Value is net chip profit from the round's initial stacks; win/tie/loss is positive/zero/negative profit, not showdown or pot outcomes." else
      if(until=="hand") "Value is side points gained in the deal; win/tie/loss means sole highest/tied highest/lower deal gains, not match victory." else
        "Value is side points gained; win/loss uses the completed match winner.",
    assumptions=c("Results depend on game_factory and continuation controllers, not universal player strength.",
      "Factories and policy closures must produce independent runs for sampling intervals to apply.",
      "Incomplete recorded runs are excluded; this can bias summaries and must be reviewed."),
    summary=if(length(summaries)) do.call(rbind,summaries) else data.frame(),
    runs=runs,statuses=statuses,games=games)
}

simulation_setup <- function(g) {
  type <- if(inherits(g,"holdem_hand")) "holdem" else if(inherits(g,"blackjack_game")) "blackjack" else
    if(inherits(g,"mus_game")) "mus" else if(inherits(g,"scopa_game")) "scopa" else stop("Unsupported simulation game")
  labels <- vapply(g$players,`[[`,"","name")
  if(type %in% c("holdem","blackjack")) {
    initial <- g$initial_stacks
    if(is.null(initial)) stop("Game needs per-seat initial stacks")
  } else {
    teams <- if(type=="mus") mus_team(seq_along(g$players)) else g$teams
    labels <- vapply(seq_len(max(teams)),function(side) paste(labels[teams==side],collapse=" & "),"")
    initial <- g$scores
  }
  list(type=type,labels=labels,initial=initial)
}

simulation_outcomes <- function(g,setup,until) {
  money <- setup$type %in% c("holdem","blackjack")
  value <- if(money) vapply(g$players,`[[`,0,"points")-setup$initial else g$scores-setup$initial
  sides <- seq_along(value)
  if(money) outcome <- ifelse(value>0,"win",ifelse(value==0,"tie","loss")) else {
    winners <- if(until=="game") g$winner else which(value==max(value))
    outcome <- ifelse(sides %in% winners,if(length(winners)>1L) "tie" else "win","loss")
  }
  data.frame(side=sides,label=setup$labels,value=value,outcome=outcome)
}
