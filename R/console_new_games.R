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

console_scopa <- function(o,controller) {
  lang <- controller$language
  text <- c(paste0("\nSCOPA | ",ui("%s to play",lang,o$player_names[o$actor])),
    ui("Deal %s | Stock %s | Scores %s",lang,o$hand_number,o$stock_count,paste(o$scores,collapse=" : ")),
    ui("Table cards",lang),if(length(o$table_cards)) console_card_rows(o$table_cards,controller$style,controller$width,TRUE) else ui("(none)",lang),
    ui("Your cards",lang),console_card_rows(o$hole,controller$style,controller$width,TRUE))
  for(i in setdiff(seq_along(o$player_names),o$actor))
    text <- c(text,paste0(o$player_names[i]," ",paste(rep("[##]",o$hand_counts[i]),collapse="")))
  for(i in seq_along(o$legal$captures)) {
    for(j in seq_along(o$legal$captures[[i]])) {
      take <- o$legal$captures[[i]][[j]]
      text <- c(text,ui("Hand %s, option %s: %s",lang,i,j,
        if(length(take)) paste(take,collapse=" + ") else ui("leave on table",lang)))
    }
  }
  c(text,ui("play HAND OPTION (table positions above)",lang),console_action_lines(o,controller))
}

console_scopa_end <- function(game,controller) {
  lang <- controller$language
  text <- "SCOPA"
  report <- utils::tail(game$results,1)[[1]]$scoring
  for(i in seq_len(nrow(report))) {
    names <- vapply(game$players[game$teams==i],`[[`,"","name")
    text <- c(text,ui("%s: cards %s | coins %s | primiera %s | settebello %s | sweeps %s | +%s",lang,
      paste(names,collapse=" & "),report$cards[i],report$coins[i],report$primiera[i],
      report$settebello[i],report$sweeps[i],report$points[i]))
  }
  text <- c(text,ui("Match scores: %s",lang,paste(game$scores,collapse=" : ")))
  if(game$complete) text <- c(text,ui("Winner: %s",lang,
    paste(vapply(game$players[game$teams==game$winner],`[[`,"","name"),collapse=" & ")))
  text
}
