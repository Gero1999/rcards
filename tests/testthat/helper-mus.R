new_mus <- function(...) mus_game(c("A","B","C","D"),...)
mus_hand <- function(values,suit="Oros") Map(card,as.character(values),rep(suit,4))
finish_mus_passing <- function(g) {
  if(g$phase=="mus") g <- mus_action(g,"cut")
  for(i in 1:16) {
    if(g$hand_complete) break
    g <- mus_action(g,"pass")
  }
  g
}

