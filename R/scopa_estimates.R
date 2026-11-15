scopa_public_model <- function(o) {
  n <- length(o$player_names)
  if(!n %in% 2:4 || !identical(as.integer(o$teams),scopa_sides(n))) stop("Invalid public Scopa seating")
  if(is.null(o$rules)) stop("Observation needs public Scopa rules")
  captured <- rep(list(list()),n);last <- NULL
  for(event in o$history) if(length(event$taken)) {
    i <- event$player
    check_number(i,"capturing seat",1,n,TRUE)
    captured[[i]] <- c(captured[[i]],list(event$card),event$taken)
    last <- i
  }
  if(!identical(vapply(captured,length,0L),as.integer(o$capture_counts))) stop("Scopa history does not account for public captured cards")
  known <- c(o$hole,o$table_cards,unlist(captured,recursive=FALSE))
  known_ids <- italian_card_ids(known)
  pack <- as_card_list(italian_deck())
  pool <- pack[!italian_card_ids(pack) %in% known_ids]
  if(sum(o$hand_counts[-o$seat])+o$stock_count!=length(pool) || length(o$hole)!=o$hand_counts[o$seat])
    stop("Public Scopa counts do not match remaining cards")
  pool <- pool[sample.int(length(pool))]
  players <- lapply(o$player_names,player)
  for(i in seq_len(n)) {
    if(i==o$seat) players[[i]]$hand <- o$hole else {
      count <- o$hand_counts[i]
      players[[i]]$hand <- utils::head(pool,count)
      if(count) pool <- pool[-seq_len(count)]
    }
  }
  structure(list(players=players,teams=o$teams,scores=o$scores,rules=do.call(scopa_rules,o$rules),
    dealer=o$dealer,actor=o$actor,stock=pool,table_cards=o$table_cards,captured=captured,
    sweeps=o$sweeps,last_capturer=last,hand_number=o$hand_number,history=o$history,
    results=list(),hand_complete=FALSE,complete=FALSE,winner=NULL),class=c("scopa_game","game"))
}

estimate_scopa_actions <- function(o,n,controllers,seed,max_actions,budget) {
  if(is.null(controllers)) controllers <- rep(list(scopa_bot()),length(o$player_names))
  controllers <- check_stat_controllers(controllers,length(o$player_names))
  candidates <- list()
  for(i in seq_along(o$legal$captures)) for(j in seq_along(o$legal$captures[[i]]))
    candidates[[length(candidates)+1L]] <- list(action="play",cards=i,amount=j)
  check_rollout_budget(n,length(candidates),budget)
  margins <- matrix(NA_real_,n,length(candidates))
  team <- o$teams[o$actor]
  with_poker_seed(seed,function() {
    for(i in seq_len(n)) {
      model <- scopa_public_model(o)
      for(j in seq_along(candidates)) {
        game <- game_action(model,candidates[[j]])
        game <- finish_stat_rollout(game,controllers,max_actions)
        points <- utils::tail(game$results,1L)[[1L]]$scoring$points
        margins[i,j] <<- points[team]-max(points[-team])
      }
    }
  })
  estimates <- do.call(rbind,lapply(seq_along(candidates),function(j) {
    move <- candidates[[j]]
    chance <- estimate_summary(as.numeric(margins[,j]>0),binary=TRUE)
    tie <- estimate_summary(as.numeric(margins[,j]==0),binary=TRUE)
    cbind(hand=move$cards,capture=move$amount,metric="deal_point_margin",estimate_summary(margins[,j]),
      lead_probability=chance$estimate,lead_lower=chance$lower,lead_upper=chance$upper,
      tie_probability=tie$estimate)
  }))
  stat_report("scopa_action_margin","monte_carlo",
    c("Public capture history reconstructs already captured cards and the last capturer.",
      "Known own/table/captured cards stay fixed; other hands and stock are uniform remaining allocations.",
      "Historical inferences about opponents' choices are omitted.",
      "The selected move is followed by the supplied controllers through the end of this deal only.",
      "Margin is this side's deal points minus the best other side; leading a deal is not winning the match.",
      "Controller state resets per action/trial and initial hidden allocations pair comparisons."),
    estimates,list(evaluations=n*length(candidates)))
}
