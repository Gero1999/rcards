console_liar <- function(o,controller) {
  lang <- controller$language
  text <- c(paste0("\nMENTIROSO | ",ui("%s to play",lang,o$player_names[o$actor])),
    ui("Pile %s | Declared rank %s",lang,o$pile_count,if(is.null(o$rank)) ui("choose a rank",lang) else o$rank),
    ui("Your cards",lang),console_card_rows(o$hole,controller$style,controller$width,TRUE))
  for(i in setdiff(seq_along(o$player_names),o$actor)) text <- c(text,
    ui("%s: %s cards",lang,o$player_names[i],o$hand_counts[i]))
  if(!is.null(o$claim)) text <- c(text,ui("%s claims %s cards of rank %s",lang,
    o$player_names[o$claim$player],o$claim$count,o$claim$rank))
  if(!is.null(o$reveal)) text <- c(text,console_liar_reveal(o$reveal,o$player_names,controller))
  if(o$phase=="claim") text <- c(text,
    ui("claim RANK POSITIONS (e.g. claim 7 1 3)",lang),
    ui("Allowed ranks: %s | Maximum cards: %s",lang,paste(o$legal$ranks,collapse=", "),o$legal$max_cards)) else
    text <- c(text,ui("Challenge the latest claim, or pass to decline.",lang))
  c(text,console_action_lines(o,controller))
}

console_liar_reveal <- function(reveal,names,controller) {
  lang <- controller$language
  c(ui(if(reveal$caught) "Caught bluff" else "Truthful claim",lang),
    ui("%s picks up %s cards",lang,names[reveal$picker],reveal$pickup_count),
    ui("Latest batch revealed",lang),console_card_rows(reveal$cards,controller$style,controller$width))
}

console_liar_end <- function(game,controller) {
  text <- "MENTIROSO"
  names <- vapply(game$players,`[[`,"","name")
  if(!is.null(game$last_reveal)) text <- c(text,console_liar_reveal(game$last_reveal,names,controller))
  c(text,ui("Winner: %s",controller$language,names[game$winner]))
}

console_liar_decision <- function(arguments,legal,controller) {
  quit_input <- function(value) !length(value) || (length(value)==1L && (is.na(value) ||
    console_action(tolower(trimws(value)),"quit",controller$language)=="quit"))
  if(!length(arguments)) {
    rank <- if(length(legal$ranks)==1L) legal$ranks else console_read(controller,
      ui("Claim rank (%s; q to quit): ",controller$language,paste(legal$ranks,collapse=", ")))
    if(quit_input(rank)) return(list(action="quit"))
    line <- console_read(controller,"Card positions (e.g. 1 3; q to quit): ")
    if(quit_input(line)) return(list(action="quit"))
    positions <- strsplit(trimws(line),"[[:space:],]+")[[1L]]
  } else {
    if(length(arguments)<2L) console_input_error("Use claim RANK POSITIONS")
    rank <- arguments[1];positions <- arguments[-1]
  }
  list(action="claim",amount=toupper(rank),cards=unname(vapply(positions,console_number,0)))
}
