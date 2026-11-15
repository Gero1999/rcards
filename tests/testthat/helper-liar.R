liar_decline <- function(g) {
  while(!g$complete && g$phase=="response") g <- liar_step(g,"pass")
  g
}

