# Rendering is called only for console controllers. Engines and simulations
# retain structured data and do not format or print cards.
console_default_write <- function(text) cat(text, "\n", sep = "")

console_card_rows <- function(cards, style = "auto", width = 80L, numbered = FALSE) {
  if (!length(cards)) return("(none)")
  unicode <- style == "unicode" || (style == "auto" && isTRUE(l10n_info()[["UTF-8"]]))
  symbols <- if (unicode) c(Hearts="\u2665",Diamonds="\u2666",Clubs="\u2663",Spades="\u2660") else
    c(Hearts="H",Diamonds="D",Clubs="C",Spades="S")
  symbols <- c(symbols,Oros="O",Copas="C",Espadas="E",Bastos="B",Denari="D",Coppe="C",Spade="S",Bastoni="B",Joker="*")
  top <- if(unicode) "\u250c\u2500\u2500\u2500\u2500\u2500\u2510" else "+-----+"
  bottom <- if(unicode) "\u2514\u2500\u2500\u2500\u2500\u2500\u2518" else "+-----+"
  edge <- if(unicode) "\u2502" else "|"
  art <- lapply(seq_along(cards),function(i) {
    x <- cards[[i]]
    rank <- if(is.null(x)) "?" else if(x$value=="Joker") "JK" else substr(x$value,1,3)
    symbol <- if(is.null(x)) "?" else unname(symbols[x$suit])
    if(is.na(symbol)) symbol <- "?"
    lines <- c(top,paste0(edge,sprintf("%-5s",rank),edge),
      paste0(edge,"  ",symbol,"  ",edge),paste0(edge,sprintf("%5s",rank),edge),bottom)
    if(numbered) lines <- c(lines,sprintf("  [%d]  ",i))
    lines
  })
  per_row <- max(1L,floor((width+1L)/8L))
  chunks <- split(seq_along(art),ceiling(seq_along(art)/per_row))
  unlist(lapply(chunks,function(indices) {
    vapply(seq_along(art[[1]]),function(row) paste(vapply(art[indices],`[[`,"",row),collapse=" "),character(1))
  }),use.names=FALSE)
}

console_opponents <- function(o, language = NULL) {
  others <- setdiff(seq_along(o$player_names),o$actor)
  paste(vapply(others,function(i) {
    back <- if(o$game_type=="holdem" && o$folded[i]) ui("folded",language) else
      paste(rep("[##]",if(o$game_type=="holdem") 2 else 4),collapse="")
    paste0(o$player_names[i]," ",back)
  },character(1)),collapse="  ")
}

console_observation_text <- function(o, controller) {
  if(o$game_type=="liar") return(console_liar(o,controller))
  if(o$game_type=="scopa") return(console_scopa(o,controller))
  if(o$game_type=="blackjack") return(console_blackjack(o,controller))
  lang <- controller$language
  phase <- if(o$game_type=="holdem") o$street else o$phase
  text <- c(paste0("\n",toupper(o$game_type)," / ",ui(toupper(phase),lang),"   ",
    ui("%s to play",lang,o$player_names[o$actor])),console_opponents(o,lang))
  if(o$game_type=="holdem") {
    text <- c(text,ui("Pot %s  |  To call %s  |  Your stack %s",lang,o$pot,o$legal$to_call,o$stacks[o$actor]))
    if(length(o$board)) text <- c(text,ui("Board",lang),console_card_rows(o$board,controller$style,controller$width))
  } else {
    partner <- which(o$teams==o$teams[o$actor] & seq_along(o$teams)!=o$actor)
    text <- c(text,ui("Hand %s  |  Partner %s  |  Scores %s : %s",lang,o$hand_number,o$player_names[partner],o$scores[1],o$scores[2]))
    if(!is.null(o$wager)) text <- c(text,ui("Stake %s",lang,if(o$ordago) "ORDAGO" else o$wager))
  }
  text <- c(text,ui("Your cards",lang),console_card_rows(o$hole,controller$style,controller$width,TRUE))
  if(!is.null(lang)) text <- c(text,paste(vapply(o$hole,function(x) translate_card(x,lang)$display,""),collapse=" / "))
  if(length(o$history)) text <- c(text,paste(vapply(utils::tail(o$history,2L),function(event) {
    detail <- if(!is.null(event$amount)) paste0(" ",event$amount) else ""
    if(!is.null(event$discard_count)) detail <- paste0(" ",event$discard_count," ",ui("cards",lang))
    paste0(o$player_names[event$player]," ",action_label(event$action,lang),detail)
  },""),collapse=" / "))
  text <- c(text,console_action_lines(o,controller))
  text
}

console_action_lines <- function(o, controller) {
  lang <- controller$language
  labels <- vapply(o$legal$actions,action_label,"",language=lang)
  text <- paste0(ui("Actions: ",lang),paste(paste0("[",seq_along(labels),"] ",labels),collapse="  "),"  |  q ",action_label("quit",lang))
  if("raise" %in% o$legal$actions && o$game_type=="holdem") text <- c(text,
    ui("raise TOTAL this round (%s..%s); already in: %s",lang,o$legal$min_raise_to,o$legal$max_raise_to,o$round_bets[o$actor]),
    ui("Total includes chips already in this round; enter the new total, not extra chips.",lang))
  if(any(c("bet","raise") %in% o$legal$actions) && o$game_type=="mus") text <- c(text,ui("bet/raise TOTAL (min %s)",lang,o$legal$min_stake))
  if("discard" %in% o$legal$actions) text <- c(text,ui("discard 1 3  |  discard none",lang))
  text
}

console_revealed_hands <- function(game) {
  if(inherits(game,"holdem_hand")) {
    seats <- which(!game$folded)
    if(!game$complete || length(seats)<2L || length(game$board)!=5L) return(integer())
    seats
  } else if(inherits(game,"mus_game") && game$hand_complete && isTRUE(game$revealed)) 1:4 else integer()
}

console_hand_label <- function(cards) {
  labels <- c("high card","one pair","two pairs","three of a kind","straight",
    "flush","full house","four of a kind","straight flush")
  category <- poker_hand_points(cards) %/% 15^5
  labels[category+1L]
}

console_hand_end_text <- function(game, controller) {
  if(inherits(game,"liar_game")) return(console_liar_end(game,controller))
  if(inherits(game,"scopa_game")) return(console_scopa_end(game,controller))
  if(inherits(game,"blackjack_game")) return(console_blackjack_end(game,controller))
  text <- character();seats <- console_revealed_hands(game)
  if(length(seats)) {
    text <- paste0("\n",ui("SHOWDOWN",controller$language))
    if(inherits(game,"holdem_hand")) text <- c(text,ui("Board",controller$language),console_card_rows(game$board,controller$style,controller$width))
    for(i in seats) text <- c(text,game$players[[i]]$name,
      console_card_rows(game$players[[i]]$hand,controller$style,controller$width))
  }
  if(inherits(game,"holdem_hand")) {
    contested <- 0L
    for(pot in game$pots) {
      names <- vapply(game$players[pot$winners],`[[`,"","name")
      if(pot$uncalled) {
        text <- c(text,ui("%s: %s back (uncalled).",controller$language,names,pot$amount));next
      }
      contested <- contested+1L
      label <- if(contested==1L) ui("pot",controller$language) else ui("side pot %s",controller$language,contested-1L)
      hand <- if(length(seats)) paste0(" - ",ui(console_hand_label(c(game$players[[pot$winners[1L]]]$hand,game$board)),controller$language)) else ""
      key <- if(length(names)>1L) "%s split %s %s%s." else if(tolower(trimws(names))=="you") "%s win %s %s%s." else "%s wins %s %s%s."
      text <- c(text,ui(key,controller$language,paste(names,collapse=" & "),label,pot$amount,hand))
    }
  } else {
    results <- Filter(function(entry) entry$hand==game$hand_number,game$results)
    summaries <- character()
    for(lance in c("grande","chica","pares","juego","punto")) {
      entries <- Filter(function(entry) entry$lance==lance,results)
      if(!length(entries)) next
      for(team in 1:2) {
        team_entries <- Filter(function(entry) entry$team==team,entries)
        if(!length(team_entries)) next
        points <- sum(vapply(team_entries,`[[`,0,"points"))
        names <- vapply(game$players[mus_team(1:4)==team],`[[`,"","name")
        summaries <- c(summaries,paste0(if(console_language(controller$language)=="en") lance else ui(toupper(lance),controller$language),": ",paste(names,collapse=" & ")," +",points))
      }
    }
    text <- c(text,strwrap(paste(summaries,collapse=" | "),width=controller$width),
      ui("Scores %s : %s",controller$language,game$scores[1L],game$scores[2L]))
    if(game$complete) {
      names <- vapply(game$players[mus_team(1:4)==game$winner],`[[`,"","name")
      text <- c(text,ui("%s win!",controller$language,paste(names,collapse=" & ")))
    }
  }
  text
}

console_notify <- function(game, controllers, status) {
  written <- list()
  for(controller in controllers) {
    if(!inherits(controller,"console_player")) next
    if(any(vapply(written,function(entry) identical(entry$write,controller$write) &&
      identical(entry$language,console_language(controller$language)),logical(1)))) next
    text <- if(status %in% c("complete","hand_complete")) console_hand_end_text(game,controller) else
      c(ui("Paused: %s. Resume with play(result$game, result$controllers).",controller$language,ui(status,controller$language)),game_summary_text(game_result(game),controller$language))
    controller$write(paste(text,collapse="\n"))
    written[[length(written)+1L]] <- list(write=controller$write,language=console_language(controller$language))
  }
  invisible(NULL)
}
