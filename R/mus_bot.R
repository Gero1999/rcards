#' Create a Mus bot for human or automated play
#'
#' Uses bounded sampling of unseen cards for partnership lance strength, together
#' with explicit exchange/wager heuristics. Samples respect current public
#' pares/juego declarations when possible; accepted sample count is reported in
#' controller state. No opponent or partner hidden cards are inspected.
#'
#' This is a baseline strategy, not an optimal Mus player. Unknown cards are
#' uniform, not inferred hand ranges. It cuts after at most two exchange rounds
#' and retains risk/decision state across hands. No table signals are modeled.
#' @param risk Baseline risk tolerance, zero to one.
#' @param adaptability Adjustment rate toward an opponent-aggression risk target.
#' @param variation Maximum per-decision deviation from the current risk.
#' @param equity_samples Desired number of sampled partnerships (one to 1000).
#' At most ten times this many candidates are attempted under public constraints.
#' @param seed Optional reproducible seed preserving the caller's RNG.
#' @return A mus_bot controller for play(). Returned controllers retain memory.
#' @export
mus_bot <- function(risk=0.5,adaptability=0.15,variation=0.05,
                    equity_samples=32L,seed=NULL) {
  for(name in c("risk","adaptability","variation")) check_number(get(name),name,0,1)
  check_number(equity_samples,"equity_samples",1,1000,TRUE)
  if(!is.null(seed)) check_number(seed,"seed",0,.Machine$integer.max,TRUE)
  structure(list(risk=risk,current_risk=risk,adaptability=adaptability,
    variation=variation,equity_samples=equity_samples,seed=seed,decisions=0L,
    last_estimate=NULL),class=c("mus_bot","game_controller"))
}

mus_bot_estimate <- function(o,samples) {
  pack <- mus_pack()
  ids <- mus_card_ids(pack)
  known <- mus_card_ids(o$hole)
  remaining <- which(!ids %in% known)
  ranks <- as.integer(vapply(pack,`[[`,"","value"))
  if(o$rules$kings==8L) {ranks[ranks==3L] <- 12L;ranks[ranks==2L] <- 1L}
  own <- mus_hand_info(o$hole,o$rules)
  order <- mus_order(o$mano);team <- mus_team(o$actor)
  others <- setdiff(1:4,o$actor)
  totals <- setNames(numeric(5),c("grande","chica","pares","juego","punto"))
  accepted <- 0L;attempted <- 0L
  while(accepted<samples && attempted<samples*10L) {
    attempted <- attempted+1L
    selected <- remaining[sample.int(length(remaining),12L)]
    info <- vector("list",4L);info[[o$actor]] <- own
    for(j in seq_along(others)) info[[others[j]]] <- mus_rank_info(ranks[selected[seq.int(4L*j-3L,4L*j)]])
    # Eligibility declarations are public; use them, never the engine's hands.
    if(o$phase %in% c("pares","juego","punto")) {
      field <- if(o$phase=="pares") "pairs_points" else "juego_points"
      eligible <- which(vapply(info,`[[`,0,field)>0)
      expected <- if(o$phase=="punto") integer() else o$eligible
      if(!setequal(eligible,expected)) next
    }
    accepted <- accepted+1L
    for(lance in names(totals)) {
      eligible <- 1:4
      if(lance=="pares") eligible <- which(vapply(info,`[[`,0,"pairs_points")>0)
      if(lance=="juego") eligible <- which(vapply(info,`[[`,0,"juego_points")>0)
      if(!length(eligible)) {totals[lance] <- totals[lance]+0.5;next}
      seats <- order[order %in% eligible]
      strength <- vapply(info[seats],`[[`,0,lance)
      winner <- seats[which.max(strength)]
      totals[lance] <- totals[lance]+as.integer(mus_team(winner)==team)
    }
  }
  # A zero-acceptance sample is explicitly neutral, never reported as certainty.
  list(strength=if(accepted) totals/accepted else setNames(rep(0.5,5L),names(totals)),
    samples=accepted,attempts=attempted)
}

mus_bot_discards <- function(o) {
  ranks <- mus_values(o$hole,o$rules)
  info <- mus_rank_info(ranks)
  if(info$pairs_points==3L || info$punto %in% c(31L,32L)) return(integer())
  counts <- tabulate(ranks,nbins=12L)
  paired <- which(counts>=2L)
  if(length(paired)) keep <- which(ranks %in% paired) else {
    keep <- which(ranks>=10L)
    if(!length(keep)) keep <- which.min(ranks)
  }
  setdiff(1:4,keep)
}

#' @export
choose_action.mus_bot <- function(controller,observation) {
  o <- observation
  if(!identical(o$game_type,"mus")) stop("Mus bots support Mus only")
  if(is.null(o$actor) || !length(o$legal$actions)) stop("No active Mus decision")
  recent <- utils::tail(Filter(function(event) {
    event$hand==o$hand_number && mus_team(event$player)!=mus_team(o$actor)
  },o$history),20L)
  if(length(recent)) {
    aggression <- mean(vapply(recent,function(event) event$action %in% c("bet","raise","ordago"),logical(1)))
    target <- max(0,min(1,controller$risk+0.3*(0.5-aggression)))
    controller$current_risk <- controller$current_risk+controller$adaptability*(target-controller$current_risk)
  }
  seed <- if(is.null(controller$seed)) NULL else (controller$seed+as.double(controller$decisions)) %% .Machine$integer.max
  choice <- with_poker_seed(seed,function() {
    risk <- max(0,min(1,controller$current_risk+stats::runif(1,-controller$variation,controller$variation)))
    if(o$phase=="exchange") return(list(decision=list(action="discard",cards=mus_bot_discards(o)),estimate=NULL))
    estimate <- mus_bot_estimate(o,controller$equity_samples)
    strength <- estimate$strength
    if(o$phase=="mus") {
      votes <- sum(vapply(o$history,function(event) {
        event$hand==o$hand_number && event$action=="mus"
      },logical(1)))
      best <- max(strength[c("grande","chica","pares","juego")])
      action <- if(votes>=8L || best>=0.65-0.15*risk) "cut" else "mus"
      return(list(decision=list(action=action),estimate=estimate))
    }
    equity <- strength[o$phase]
    legal <- o$legal$actions
    if("accept" %in% legal) {
      threshold <- if(isTRUE(o$ordago)) 0.75-0.2*risk else 0.65-0.2*risk
      action <- if(equity>=threshold) "accept" else "reject"
      # Bound autonomous escalation by the score target to prevent raise loops.
      if("raise" %in% legal && equity>0.85 && risk>0.55 && o$legal$min_stake<=o$rules$points_to_win) {
        return(list(decision=list(action="raise",amount=o$legal$min_stake),estimate=estimate))
      }
      return(list(decision=list(action=action),estimate=estimate))
    }
    if(equity>0.65-0.15*risk) {
      amount <- o$legal$min_stake+floor(4*equity*risk)
      return(list(decision=list(action="bet",amount=amount),estimate=estimate))
    }
    list(decision=list(action="pass"),estimate=estimate)
  })
  controller$decisions <- controller$decisions+1L
  controller$last_estimate <- choice$estimate
  list(decision=choice$decision,controller=controller)
}
