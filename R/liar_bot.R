#' Create a public-information Mentiroso controller
#' @param challenge_rate Chance of challenging an otherwise uncertain claim,
#' between zero and one; default 0.2. Impossible claims are always challenged;
#' the rate doubles (capped at one) for final-card claims. This is not calibrated bluff inference.
#' @param seed Optional reproducible decision seed preserving caller RNG.
#' @return A game_controller. It samples a held rank weighted by frequency, plays matching
#' cards when possible and otherwise bluffs with one card. It sees observations
#' only and never reads the face-down pile or actual opposing hands.
#' @export
liar_bot <- function(challenge_rate=0.2,seed=NULL) {
  check_number(challenge_rate,"challenge_rate",0,1)
  if(!is.null(seed)) check_number(seed,"seed",0,.Machine$integer.max,TRUE)
  structure(list(challenge_rate=challenge_rate,seed=seed,decisions=0L),class=c("liar_bot","game_controller"))
}
#' @export
choose_action.liar_bot <- function(controller,observation) {
  o <- observation
  if(!identical(o$game_type,"liar") || is.null(o$actor) || o$seat!=o$actor) stop("Expected an active Liar observation")
  seed <- if(is.null(controller$seed)) NULL else (controller$seed+as.double(controller$decisions)) %% .Machine$integer.max
  decision <- with_poker_seed(seed,function() {
    values <- vapply(o$hole,`[[`,"","value")
    if(o$phase=="response") {
      impossible <- sum(values==o$claim$rank)+o$claim$count>4L
      final <- o$hand_counts[o$claim$player]==0L
      rate <- if(final) min(1,2*controller$challenge_rate) else controller$challenge_rate
      challenge <- impossible || stats::runif(1)<rate
      return(list(action=if(challenge) "challenge" else "pass"))
    }
    rank <- o$rank
    if(is.null(rank)) {
      counts <- base::table(factor(values,levels=o$legal$ranks))
      rank <- o$legal$ranks[sample.int(length(counts),1L,prob=as.numeric(counts))]
    }
    cards <- which(values==rank)
    if(!length(cards)) cards <- sample.int(length(o$hole),1L)
    list(action="claim",amount=rank,cards=utils::head(cards,o$legal$max_cards))
  })
  controller$decisions <- controller$decisions+1L
  list(decision=decision,controller=controller)
}
