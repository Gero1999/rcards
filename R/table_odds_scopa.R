# Estimate every legal play and capture with a shared budget for simulated turns.
table_scopa_odds_dialog <- function(observation, language = "en") {
  if (!identical(observation$game_type, "scopa")) stop("Expected a Scopa observation")
  captures <- observation$legal$captures
  moves <- sum(lengths(captures))
  if (!moves) stop("Provide the acting seat's active public observation")
  turns_remaining <- sum(observation$hand_counts) + observation$stock_count
  samples <- max(2L, min(200L, 4000L %/% (moves * turns_remaining)))
  report <- estimate_actions(observation, n = samples, seed = 42L,
    max_actions = 36L, max_evaluations = moves * samples)
  estimates <- report$estimates
  estimates <- estimates[order(-estimates$estimate, -estimates$lead_probability), ]

  describe_card <- function(card, position) {
    card$symbol <- ""
    paste0("#", position, " ", trimws(translate_card(card, language)$display))
  }
  rows <- lapply(seq_len(nrow(estimates)), function(i) {
    row <- estimates[i, ]
    take <- captures[[row$hand]][[row$capture]]
    if (length(take)) {
      description <- lapply(take, function(position) {
        shiny::tags$div(describe_card(observation$table_cards[[position]], position))
      })
    } else {
      description <- ui("No capture", language)
    }
    shiny::tags$tr(
      shiny::tags$th(scope = "row", describe_card(observation$hole[[row$hand]], row$hand)),
      shiny::tags$td(description),
      shiny::tags$td(sprintf("%.2f", row$estimate),
        shiny::tags$div(shiny::tags$small(sprintf("%.2f \u2013 %.2f", row$lower, row$upper)))),
      shiny::tags$td(sprintf("%.1f%%", 100 * row$lead_probability)))
  })

  title_id <- "rcards-scopa-odds-title"
  dialog <- shiny::modalDialog(
    title = shiny::tags$span(id = title_id, ui("Compare moves", language)),
    shiny::tags$p(ui("%s simulated deals per move.", language, samples)),
    shiny::tags$div(class = "table-responsive",
      shiny::tags$table(class = "table table-condensed",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(scope = "col", ui("Card to play", language)),
          shiny::tags$th(scope = "col", ui("Capture", language)),
          shiny::tags$th(scope = "col", `aria-sort` = "descending", ui("Expected deal margin (points)", language),
            shiny::tags$div(shiny::tags$small(ui("95% simulation interval (points)", language)))),
          shiny::tags$th(scope = "col", ui("Chance to finish the deal ahead", language)))),
        shiny::tags$tbody(rows))),
    shiny::tags$p(ui("The margin is your side's deal points minus the best other side's deal points.", language)),
    shiny::tags$p(ui("The simulation keeps visible cards and captures fixed, samples unseen cards uniformly, and uses baseline bots to finish the deal.", language)),
    footer = shiny::modalButton(table_help_text(language)$close), easyClose = TRUE)
  dialog$attribs$class <- paste(dialog$attribs$class, "rc-odds-modal rc-scopa-odds")
  dialog$attribs$role <- "dialog"
  dialog$attribs[["aria-modal"]] <- "true"
  dialog$attribs[["aria-labelledby"]] <- title_id
  dialog
}
