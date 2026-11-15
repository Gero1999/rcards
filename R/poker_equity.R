#' Estimate or enumerate Texas Hold'em showdown equity
#' @param hole Two known French cards.
#' @param board Zero, three, four or five known community cards.
#' @param opponents Total opposing hands, one through nine.
#' @param opponent_hands Optional list of known two-card opposing hands. Remaining
#' opponents receive uniform random holdings. All known cards must be distinct.
#' @param n Monte Carlo samples, one through 100000.
#' @param method auto enumerates small spaces, sample always samples, exact
#' requires enumeration within max_exact. No betting/folding is simulated.
#' @param max_exact Maximum enumerated states, one through 100000; default 10000.
#' @param seed Optional seed preserving caller RNG state.
#' @return An rcards_estimate containing win/tie/loss probabilities, equity
#' (expected pot fraction), sample counts, assumptions and uncertainty intervals.
#' @export
poker_equity <- function(hole,board=list(),opponents=1L,opponent_hands=NULL,n=1000L,
                         method=c("auto","sample","exact"),max_exact=10000L,seed=NULL) {
  method <- match.arg(method);n <- estimate_n(n);max_exact <- estimate_n(max_exact)
  check_number(opponents,"opponents",1,9,TRUE)
  if(length(hole)!=2L || !length(board) %in% c(0L,3:5)) stop("Expected two hole cards and a legal board")
  if(is.null(opponent_hands)) opponent_hands <- list()
  if(!is.list(opponent_hands) || length(opponent_hands)>opponents ||
     any(vapply(opponent_hands,length,0L)!=2L)) stop("Provide at most opponents known two-card hands")
  known <- c(as_card_list(hole),as_card_list(board),unlist(lapply(opponent_hands,as_card_list),recursive=FALSE))
  known_ids <- poker_card_ids(known)
  pack <- as_card_list(poker_pack());ids <- poker_card_ids(pack)
  ranks <- match(vapply(pack,`[[`,"","value"),c(as.character(2:10),"J","Q","K","A"))+1L
  suits <- match(vapply(pack,`[[`,"","suit"),c("Hearts","Diamonds","Clubs","Spades"))
  hero <- match(poker_card_ids(hole),ids);fixed <- match(poker_card_ids(board),ids)
  enemies <- lapply(opponent_hands,function(hand) match(poker_card_ids(hand),ids))
  available <- which(!ids %in% known_ids)
  missing <- 5L-length(board);unknown <- opponents-length(enemies)
  if(missing+2L*unknown>length(available)) stop("Not enough unknown cards")
  states <- choose(length(available),missing)
  if(unknown) for(j in seq_len(unknown)) states <- states*choose(length(available)-missing-2L*(j-1L),2L)
  exact <- method=="exact" || (method=="auto" && states<=max_exact)
  if(exact && states>max_exact) stop("Exact enumeration exceeds max_exact; use sample or raise the budget")
  count <- if(exact) as.integer(states) else n
  output <- matrix(0,nrow=count,ncol=4,dimnames=list(NULL,c("win","tie","loss","equity")))
  row <- 0L
  evaluate <- function(community,others) {
    hero_score <- poker_score_values(ranks[c(hero,community)],suits[c(hero,community)])
    scores <- vapply(others,function(hand) poker_score_values(ranks[c(hand,community)],suits[c(hand,community)]),0)
    best <- max(c(hero_score,scores));tied <- sum(scores==hero_score)
    share <- if(hero_score==best) 1/(1L+tied) else 0
    row <<- row+1L
    output[row,] <<- c(as.integer(share==1),as.integer(share>0 && share<1),as.integer(share==0),share)
  }
  with_poker_seed(seed,function() {
    if(!exact) for(i in seq_len(n)) {
      draw <- available[sample.int(length(available),missing+2L*unknown)]
      community <- c(fixed,utils::head(draw,missing));others <- enemies
      if(unknown) for(j in seq_len(unknown)) others[[length(others)+1L]] <- draw[missing+c(2L*j-1L,2L*j)]
      evaluate(community,others)
    } else {
      enumerate_opponents <- function(remaining,others,community,left) {
        if(!left) {evaluate(community,others);return(invisible(NULL))}
        for(pair in utils::combn(remaining,2L,simplify=FALSE))
          enumerate_opponents(setdiff(remaining,pair),c(others,list(pair)),community,left-1L)
        invisible(NULL)
      }
      boards <- if(missing) utils::combn(available,missing,simplify=FALSE) else list(integer())
      for(extra in boards) enumerate_opponents(setdiff(available,extra),enemies,c(fixed,extra),unknown)
    }
  })
  estimates <- do.call(rbind,lapply(seq_len(4),function(j)
    cbind(metric=colnames(output)[j],estimate_summary(output[,j],binary=j<4,exact=exact,bounds=c(0,1)))))
  stat_report("poker_showdown",if(exact) "exact" else "monte_carlo",
    c("Unknown holdings and board completions are uniform without replacement.",
      "No folding, betting history, side-pot amounts or opponent ranges are modeled.",
      "Equity is expected pot share; tie is a shared best hand."),estimates,
    list(possible_states=states,opponents=opponents,known_opponents=length(enemies)))
}

estimate_poker_observation <- function(o,...) {
  if(is.null(o$seat) || is.null(o$actor) || o$seat!=o$actor) stop("Estimate the acting seat's observation")
  poker_equity(o$hole,o$board,opponents=sum(!o$folded)-1L,...)
}
