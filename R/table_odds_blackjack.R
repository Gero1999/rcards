# Run a bounded, reproducible comparison from the acting player's observation.
# The estimator samples its own shoe and preserves the caller's RNG state.
table_blackjack_odds_dialog <- function(observation, language = "en") {
  if (!identical(observation$game_type, "blackjack")) stop("Expected a Blackjack observation")
  samples <- 500L
  report <- estimate_actions(observation, n = samples, seed = 42L,
    max_actions = 100L, max_evaluations = 4L * samples)
  estimates <- report$estimates
  labels <- c("Action", "Expected net profit (chips)", "95% simulation interval (chips)",
    "Chance of profit")

  rows <- lapply(seq_len(nrow(estimates)), function(i) {
    row <- estimates[i, ]
    shiny::tags$tr(
      shiny::tags$th(scope = "row", action_label(row$action, language)),
      shiny::tags$td(sprintf("%.2f", row$estimate)),
      shiny::tags$td(sprintf("%.2f \u2013 %.2f", row$lower, row$upper)),
      shiny::tags$td(sprintf("%.1f%%", 100 * row$profit_probability)))
  })

  title_id <- "rcards-blackjack-odds-title"
  dialog <- shiny::modalDialog(
    title = shiny::tags$span(id = title_id, ui("Compare actions", language)),
    shiny::tags$p(ui("%s simulated rounds per action.", language, samples)),
    shiny::tags$div(class = "table-responsive",
      shiny::tags$table(class = "table table-condensed",
        shiny::tags$thead(shiny::tags$tr(lapply(labels, function(label) {
          shiny::tags$th(scope = "col", ui(label, language))
        }))),
        shiny::tags$tbody(rows))),
    shiny::tags$p(ui("Unknown cards are sampled from the configured shoe using your visible cards and the dealer's blackjack check. A baseline bot makes any remaining player decisions.", language)),
    shiny::tags$p(ui("The comparison models your seat against the dealer. Net profit includes all your wagers, including doubles and splits.", language)),
    footer = shiny::modalButton(table_help_text(language)$close), easyClose = TRUE)
  dialog$attribs$class <- paste(dialog$attribs$class, "rc-odds-modal")
  dialog$attribs$role <- "dialog"
  dialog$attribs[["aria-modal"]] <- "true"
  dialog$attribs[["aria-labelledby"]] <- title_id
  dialog
}
