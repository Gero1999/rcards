#' Estimate Mus partnership strength for each lance
#' @param observation An active mus_observation() for the acting seat.
#' @param n Requested accepted deals, one through 100000.
#' @param seed Optional reproducible seed preserving caller RNG.
#' @return An rcards_estimate with conditional win probabilities by lance,
#' applicable sample counts/rates, accepted deals, attempts and exhaustion status.
#' Ties follow mano order. These are comparison probabilities, not probabilities
#' of accepting bets, winning points or winning the match.
#' @export
mus_lance_equity <- function(observation,n=1000L,seed=NULL) {
  o <- observation;n <- estimate_n(n)
  if(!identical(o$game_type,"mus") || is.null(o$actor) || is.null(o$seat) ||
     o$seat!=o$actor || length(o$hole)!=4L) stop("Provide the acting seat's active Mus observation")
  known <- mus_card_ids(o$hole);pack <- as_card_list(mus_pack())
  remaining <- pack[!mus_card_ids(pack) %in% known]
  own <- mus_hand_info(o$hole,o$rules);others <- setdiff(1:4,o$actor)
  order <- mus_order(o$mano);team <- mus_team(o$actor)
  lances <- c("grande","chica","pares","juego","punto")
  values <- matrix(NA_real_,n,5L,dimnames=list(NULL,lances))
  accepted <- 0L;attempts <- 0L
  with_poker_seed(seed,function() {
    while(accepted<n && attempts<n*10L) {
      attempts <<- attempts+1L
      draw <- remaining[sample.int(length(remaining),12L)]
      info <- vector("list",4L);info[[o$actor]] <- own
      for(j in seq_along(others)) info[[others[j]]] <- mus_hand_info(draw[seq.int(4L*j-3L,4L*j)],o$rules)
      pairs <- which(vapply(info,`[[`,0,"pairs_points")>0)
      juego <- which(vapply(info,`[[`,0,"juego_points")>0)
      if(o$phase %in% c("pares","juego","punto")) {
        actual <- if(o$phase=="pares") pairs else juego
        expected <- if(o$phase=="punto") integer() else o$eligible
        if(!setequal(actual,expected)) next
      }
      accepted <<- accepted+1L
      for(lance in lances) {
        eligible <- switch(lance,pares=pairs,juego=juego,punto=if(!length(juego)) 1:4 else integer(),1:4)
        if(!length(eligible)) next
        seats <- order[order %in% eligible]
        winner <- seats[which.max(vapply(info[seats],`[[`,0,lance))]
        values[accepted,lance] <<- as.integer(mus_team(winner)==team)
      }
    }
  })
  estimates <- do.call(rbind,lapply(lances,function(lance) {
    observations <- if(accepted) values[seq_len(accepted),lance] else numeric()
    cbind(lance=lance,estimate_summary(observations,binary=TRUE),
      applicable_rate=if(accepted) mean(!is.na(observations)) else NA_real_)
  }))
  stat_report("mus_lance_strength","monte_carlo",
    c("Unknown hands are uniform, conditioned on current public pares/juego declarations.",
      "Own four cards and configured kings/mano rules are respected; discard and betting inference is omitted.",
      "Win probabilities are conditional on the lance being applicable; missing estimates are not 50%.",
      "No wagers, refusal behavior, point settlement or match strategy are simulated."),estimates,
    list(accepted=accepted,attempts=attempts,requested=n,exhausted=accepted<n))
}
