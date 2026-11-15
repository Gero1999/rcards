console_blackjack <- function(o,controller) {
  lang <- controller$language
  text <- c(paste0("\nBLACKJACK | ",ui("%s to play",lang,o$player_names[o$actor])),
    ui("Dealer",lang),console_card_rows(o$dealer,controller$style,controller$width))
  for(j in seq_along(o$hands)) {
    h <- o$hands[[j]]
    text <- c(text,ui("Hand %s | Total %s | Wager %s",lang,j,blackjack_total(h$cards)$total,h$bet),
      console_card_rows(h$cards,controller$style,controller$width))
  }
  c(text,ui("Active hand %s | Your stack %s",lang,o$active_hand,o$stacks[o$actor]),console_action_lines(o,controller))
}

console_blackjack_end <- function(game,controller) {
  lang <- controller$language
  text <- c("BLACKJACK",ui("Dealer",lang),console_card_rows(game$dealer,controller$style,controller$width),
    ui("Total %s",lang,blackjack_total(game$dealer)$total))
  for(result in game$results) {
    i <- result$player;j <- result$hand
    text <- c(text,game$players[[i]]$name,console_card_rows(game$hands[[i]][[j]]$cards,controller$style,controller$width),
      ui("Hand %s: %s | Returned %s",lang,j,ui(result$outcome,lang),result$returned))
  }
  text
}
