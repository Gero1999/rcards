# Use settled payouts so that split-pot odd chips come from the engine. An
# uncalled return is part of the credit to a stack but is not a pot award.
table_pot_event <- function(game, seat) {
  if (!inherits(game, "holdem_hand") || !game_hand_complete(game)) return(NULL)
  result <- game_result(game)
  awards <- result$payouts
  for (pot in game$pots) {
    if (isTRUE(pot$uncalled)) awards[pot$winners] <- awards[pot$winners] - pot$amount
  }
  winners <- which(awards > 0)
  if (!length(winners)) return(NULL)

  viewer_won <- seat %in% winners
  title_args <- list()
  if (length(winners) == 1L) {
    title <- if (viewer_won) "You win the pot" else "%s wins the pot"
    if (!viewer_won) title_args <- list(result$player_names[[winners]])
  } else {
    contested <- Filter(function(pot) !isTRUE(pot$uncalled), game$pots)
    all_split <- all(vapply(contested, function(pot) setequal(pot$winners, winners), logical(1)))
    title <- if (all_split) "Split pot" else "Pots awarded"
  }

  detail <- lapply(winners, function(i) {
    amount <- format(awards[[i]], scientific = FALSE, trim = TRUE)
    list(key = "%s: %s chips", args = list(result$player_names[[i]], amount))
  })
  list(key = paste0("holdem-pot-", result$hand_number), title = title,
    title_args = title_args, detail = detail, tone = if (viewer_won) "win" else "neutral",
    pot_winners = winners)
}

table_event_tag <- function(event, language = "en") {
  if (is.null(event)) return(NULL)
  event_text <- function(key, args) do.call(ui, c(list(key, language), args))
  classes <- if (identical(event$tone, "win")) "rc-event rc-event-win" else "rc-event"
  shiny::tags$div(class = classes,
    `data-rc-table-event` = event$key,
    `data-rc-pot-winners` = if (length(event$pot_winners)) paste(event$pot_winners, collapse = ","),
    shiny::tags$div(class = "rc-event-title", event_text(event$title, event$title_args)),
    lapply(event$detail, function(line) {
      shiny::tags$div(class = "rc-event-detail", event_text(line$key, line$args))
    }))
}
