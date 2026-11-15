#' Configure Scopa match scoring
#' @param target Positive integer target, default eleven. A tied lead requires
#' another deal; matches finish only after a deal has settled.
#' @return A validated scopa_rules object.
#' @export
scopa_rules <- function(target=11L) {
  check_number(target,"target",1,integer=TRUE)
  structure(list(target=target),class="scopa_rules")
}

scopa_primiera <- function(cards) {
  suits <- c("Denari","Coppe","Spade","Bastoni")
  values <- c(16,12,13,14,15,18,21,10,10,10)
  if(!all(suits %in% vapply(cards,`[[`,"","suit"))) return(NA_real_)
  sum(vapply(suits,function(suit) {
    ranks <- as.integer(vapply(Filter(function(x) x$suit==suit,cards),`[[`,"","value"))
    max(values[ranks])
  },0))
}

scopa_majority <- function(values) {
  valid <- which(!is.na(values))
  if(!length(valid)) return(integer())
  winners <- valid[values[valid]==max(values[valid])]
  if(length(winners)==1L) winners else integer()
}

scopa_deal_score <- function(captured,teams,sweeps) {
  sides <- seq_len(max(teams))
  piles <- lapply(sides,function(team) unlist(captured[teams==team],recursive=FALSE))
  report <- data.frame(side=sides,cards=vapply(piles,length,0L),
    coins=vapply(piles,function(cards) sum(vapply(cards,function(x) x$suit=="Denari",logical(1))),0L),
    settebello=vapply(piles,function(cards) as.integer(any(vapply(cards,function(x) x$suit=="Denari" && x$value=="7",logical(1)))),0L),
    primiera=vapply(piles,scopa_primiera,0),
    sweeps=vapply(sides,function(team) sum(sweeps[teams==team]),0L))
  report$points <- report$settebello+report$sweeps
  for(category in c("cards","coins","primiera")) {
    winner <- scopa_majority(report[[category]])
    report$points[winner] <- report$points[winner]+1L
  }
  report
}

scopa_settle <- function(g) {
  report <- scopa_deal_score(g$captured,g$teams,g$sweeps)
  g$scores <- g$scores+report$points
  g$results[[length(g$results)+1L]] <- list(hand=g$hand_number,scoring=report,scores=g$scores)
  leaders <- which(g$scores==max(g$scores))
  if(max(g$scores)>=g$rules$target && length(leaders)==1L) {
    g$complete <- TRUE;g$winner <- leaders
  }
  g
}

scopa_next_deal <- function(g,seed=NULL,pack=NULL) {
  if(!g$hand_complete || g$complete) stop("Finish the current deal; match must not be complete")
  next_game <- new_scopa(g$players,g$rules,(g$dealer %% length(g$players))+1L,seed,pack)
  next_game$hand_number <- g$hand_number+1L
  next_game$scores <- g$scores;next_game$results <- g$results
  next_game
}
