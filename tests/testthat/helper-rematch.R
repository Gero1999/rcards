finished_liar_for_rematch <- function(rules=liar_rules(),dealer=3L) {
  g <- liar_game(c("A","B","C"),rules=rules,dealer=dealer,seed=3)
  actor <- g$actor
  receiver <- clockwise_seats(length(g$players),actor)[1L]
  g$players[[receiver]]$hand <- c(g$players[[receiver]]$hand,g$players[[actor]]$hand[-1L])
  g$players[[actor]]$hand <- g$players[[actor]]$hand[1L]
  rank <- g$players[[actor]]$hand[[1L]]$value
  liar_step(liar_step(g,"claim",1L,rank),"challenge")
}

finished_mus_for_rematch <- function() {
  g <- mus_game(c("A","B","C","D"),rules=mus_rules(4,30),mano=4L,seed=2)
  mus_action(mus_action(mus_action(g,"cut"),"ordago"),"accept")
}

finished_scopa_for_rematch <- function(n=3L) {
  g <- scopa_game(paste0("P",seq_len(n)),rules=scopa_rules(target=1),dealer=n,seed=3)
  for(i in seq_len(360L)) {
    if(g$complete) return(g)
    if(g$hand_complete) g <- scopa_next_deal(g,seed=g$hand_number+3L)
    g <- scopa_step(g,1L,1L)
  }
  stop("Scopa fixture did not finish")
}

