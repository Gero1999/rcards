# Announcements carry public labels and outcomes, with keys independent of
# language, session pause state, and reactive redraws.
table_other_event <- function(game, seat) {
  if (inherits(game, "blackjack_game")) return(table_blackjack_event(game, seat))
  if (inherits(game, c("mus_game", "scopa_game")) && game_complete(game)) {
    return(table_match_event(game_result(game), seat))
  }
  if (inherits(game, "scopa_game")) return(table_scopa_sweep_event(game, seat))
  if (inherits(game, "liar_game")) return(table_liar_event(game, seat))
  NULL
}

table_blackjack_event <- function(game, seat) {
  if (!game_hand_complete(game)) return(NULL)
  result <- game_result(game)
  hands <- Filter(function(hand) hand$player == seat, result$results)
  if (!length(hands)) return(NULL)

  net <- sum(vapply(hands, function(hand) hand$returned - hand$bet, 0))
  signed_net <- paste0(if (net > 0) "+" else "", format(net, trim = TRUE, scientific = FALSE))
  detail <- list(list(key = "%s chips this round", args = list(signed_net)))
  player_natural <- any(vapply(hands, function(hand) isTRUE(hand$natural), logical(1)))
  dealer_natural <- blackjack_total(game_observation(game, seat)$dealer)$natural

  if (player_natural || dealer_natural) {
    title <- "Blackjack!"
    natural <- if (player_natural && dealer_natural) "You and the dealer have blackjack." else
      if (player_natural) "You have blackjack." else "The dealer has blackjack."
    detail <- c(list(list(key = natural)), detail)
  } else {
    title <- if (net > 0) "You won the round!" else if (net < 0) "You lost the round" else "You broke even"
  }
  list(key = paste0("blackjack-round-", result$round_number), title = title, detail = detail,
    tone = if (net > 0) "win" else "neutral")
}

table_match_event <- function(result, seat) {
  winning_seats <- which(result$teams == result$winner)
  won <- seat %in% winning_seats
  winners <- paste(result$player_names[winning_seats], collapse = " & ")
  list(key = paste(result$game_type, "match", result$hand_number, sep = "-"),
    title = if (won) "You won the match!" else "%s won the match!",
    title_args = if (won) list() else list(winners),
    detail = list(list(key = "Final score %s", args = list(paste(result$scores, collapse = " \u2013 ")))),
    tone = if (won) "win" else "neutral")
}

table_scopa_sweep_event <- function(game, seat) {
  turn <- length(game$history)
  if (!turn) return(NULL)
  entry <- game$history[[turn]]
  if (!isTRUE(entry$sweep)) return(NULL)

  detail <- if (entry$player == seat) list(key = "You cleared the table for 1 point.") else
    list(key = "%s cleared the table for 1 point.", args = list(game$players[[entry$player]]$name))
  list(key = paste("scopa-sweep", game$hand_number, turn, sep = "-"), title = "Scopa!",
    detail = list(detail), tone = if (game$teams[[entry$player]] == game$teams[[seat]]) "win" else "neutral")
}

table_liar_event <- function(game, seat) {
  observation <- game_observation(game, seat)
  if (observation$complete) {
    won <- observation$winner == seat
    return(list(key = paste0("liar-deal-", observation$deal_number),
      title = if (won) "You won the deal!" else "%s won the deal!",
      title_args = if (won) list() else list(observation$player_names[[observation$winner]]),
      tone = if (won) "win" else "neutral"))
  }

  turn <- length(observation$history)
  if (!turn) return(NULL)
  entry <- observation$history[[turn]]
  if (!identical(entry$action, "challenge") || !isTRUE(entry$caught)) return(NULL)

  challenger <- observation$player_names[[entry$player]]
  claimant <- observation$player_names[[entry$claimant]]
  detail <- if (entry$player == seat) list(key = "You caught %s bluffing.", args = list(claimant)) else
    if (entry$claimant == seat) list(key = "%s caught your bluff.", args = list(challenger)) else
      list(key = "%s caught %s bluffing.", args = list(challenger, claimant))
  list(key = paste("liar-bluff", observation$deal_number, turn, sep = "-"), title = "Bluff caught!",
    detail = list(detail), tone = if (entry$player == seat) "win" else "neutral")
}
