# Capture only the player's own cards, public board, and live opponent count.
table_holdem_odds_snapshot <- function(observation) {
  if (!identical(observation$game_type, "holdem") || isTRUE(observation$complete) ||
      !isTRUE(observation$seat == observation$actor)) return(NULL)
  hole <- poker_card_ids(as_card_list(observation$hole))
  board <- poker_card_ids(as_card_list(observation$board))
  list(slots = c(hole, board, rep("", 5L - length(board))),
       opponents = sum(!observation$folded & observation$hand_counts == 2L) - 1L)
}

table_holdem_odds_scenario <- function(slots, cards, language) {
  hole <- slots[1:2]
  board <- slots[3:7]
  if (any(!nzchar(hole))) stop(ui("Choose two hole cards.", language))
  if (any(nzchar(board)) && any(!nzchar(board[1:3]))) {
    stop(ui("Choose all three flop cards.", language))
  }
  if (nzchar(board[5L]) && !nzchar(board[4L])) {
    stop(ui("Choose a turn card before the river.", language))
  }
  board <- board[nzchar(board)]
  known <- c(hole, board)
  if (!all(known %in% names(cards))) stop(ui("Choose cards from the standard French deck.", language))
  if (anyDuplicated(known)) stop(ui("Each card can appear only once.", language))
  list(hole = unname(cards[hole]), board = unname(cards[board]))
}

# Five cards of one suit can be available in at most one suit among seven
# cards. Sum those disjoint probabilities over the four possible suits.
table_holdem_flush_chance <- function(hole, board) {
  known <- c(hole, board)
  suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
  counts <- tabulate(match(vapply(known, `[[`, "", "suit"), suits), nbins = length(suits))
  remaining <- 52L - length(known)
  suited <- 13L - counts
  sum(stats::phyper(4L - counts, suited, remaining - suited, 5L - length(board), lower.tail = FALSE))
}

table_holdem_odds_result <- function(result, language) {
  report <- result$report
  exact <- identical(report$method, "exact")
  rows <- report$estimates[match(c("win", "tie", "loss"), report$estimates$metric), ]
  labels <- c("Win at showdown", "Tie at showdown", "Lose at showdown")
  percent <- function(value) sprintf("%.1f%%", 100 * value)
  shiny::tags$div(class = "rc-odds-results",
    shiny::tags$h4(ui(if (result$edited) "What if?" else "Current hand", language)),
    shiny::tags$dl(class = "rc-odds-metrics", lapply(seq_len(nrow(rows)), function(i) {
      interval <- if (!exact) ui("95%% simulation interval: %s to %s.", language,
        percent(rows$lower[[i]]), percent(rows$upper[[i]]))
      shiny::tagList(
        shiny::tags$dt(ui(labels[[i]], language)),
        shiny::tags$dd(percent(rows$estimate[[i]]),
          if (!is.null(interval)) shiny::tags$small(paste0(" ", interval))))
    })),
    shiny::tags$p(ui(if (exact) "Exact showdown calculation (%s outcomes)." else
      "Simulated showdown odds (%s samples).", language, rows$samples[[1L]])),
    shiny::tags$p(ui("A tie means sharing the best hand at showdown.", language)),
    shiny::tags$p(ui("Unknown opposing cards and board cards are drawn at random. Betting and side pots are excluded.", language)),
    shiny::tags$dl(class = "rc-odds-metrics",
      shiny::tags$dt(ui("Flush by the river", language)),
      shiny::tags$dd(percent(result$flush))),
    shiny::tags$p(ui("Five cards of one suit among your seven cards, including straight flushes.", language)))
}

table_holdem_odds_dialog <- function(ns, snapshot, cards, language) {
  values <- stats::setNames(names(cards), vapply(cards, function(card) {
    translate_card(card, language)$display
  }, ""))
  labels <- c(rep("Hole card %s", 2L), rep("Flop card %s", 3L), "Turn card", "River card")
  label_args <- c(1L, 2L, 1:3)
  slot <- function(i) {
    heading <- if (i <= 5L) ui(labels[[i]], language, label_args[[i]]) else ui(labels[[i]], language)
    choices <- if (i <= 2L) values else c(stats::setNames("", ui("Unknown", language)), values)
    shiny::selectInput(ns(paste0("card", i)), heading, choices, selected = snapshot$slots[[i]],
      selectize = FALSE)
  }
  row <- function(indices, width) shiny::tags$div(class = "row", lapply(indices, function(i) {
    shiny::tags$div(class = paste0("col-xs-", width), slot(i))
  }))
  dialog <- shiny::modalDialog(
    title = shiny::tags$span(id = ns("title"), ui("Hand odds", language)),
    shiny::tags$p(ui("Opponents in this hand: %s", language, snapshot$opponents)),
    shiny::tags$div(class = "rc-odds-card-grid", row(1:2, 6L), row(3:5, 4L), row(6:7, 6L)),
    shiny::tags$div(class = "rc-odds-controls",
      shiny::actionButton(ns("calculate"), ui("Calculate odds", language), class = "btn-primary"),
      shiny::actionButton(ns("reset"), ui("Reset to current hand", language))),
    shiny::tags$div(`aria-live` = "polite", shiny::uiOutput(ns("result"))),
    footer = shiny::modalButton(ui("Back to the table", language)), easyClose = TRUE)
  dialog$attribs$role <- "dialog"
  dialog$attribs$class <- paste(dialog$attribs$class, "rc-odds-modal")
  dialog$attribs[["aria-modal"]] <- "true"
  dialog$attribs[["aria-labelledby"]] <- ns("title")
  dialog
}

table_holdem_odds_server <- function(id, language) {
  shiny::moduleServer(id, function(input, output, session) {
    cards <- as_card_list(poker_pack())
    names(cards) <- poker_card_ids(cards)
    snapshot <- shiny::reactiveVal(NULL)
    result <- shiny::reactiveVal(NULL)
    slot_ids <- paste0("card", 1:7)
    edited_slots <- shiny::reactive({
      slots <- lapply(slot_ids, function(id) input[[id]])
      shiny::req(all(lengths(slots) == 1L))
      unlist(slots, use.names = FALSE)
    })
    calculate <- function(slots, current) {
      tryCatch({
        scenario <- table_holdem_odds_scenario(slots, cards, language())
        report <- poker_equity(scenario$hole, scenario$board, current$opponents,
          n = 1500L, max_exact = 2500L, seed = 42L)
        list(slots = slots, edited = !identical(slots, current$slots), report = report,
          flush = table_holdem_flush_chance(scenario$hole, scenario$board))
      }, error = function(error) list(slots = slots, error = conditionMessage(error)))
    }
    shiny::observeEvent(input$calculate, {
      shiny::req(snapshot())
      result(calculate(edited_slots(), snapshot()))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$reset, {
      current <- snapshot()
      shiny::req(current)
      for (i in seq_along(slot_ids)) {
        shiny::updateSelectInput(session, slot_ids[[i]], selected = current$slots[[i]])
      }
      result(current$result)
    }, ignoreInit = TRUE)
    output$result <- shiny::renderUI({
      current <- result()
      shiny::req(current)
      if (!identical(edited_slots(), current$slots)) {
        return(shiny::tags$p(ui("Select Calculate odds to update the result.", language())))
      }
      if (!is.null(current$error)) return(shiny::tags$p(class = "rc-error", role = "alert", current$error))
      table_holdem_odds_result(current, language())
    })
    function(observation) {
      current <- table_holdem_odds_snapshot(observation)
      if (is.null(current) || current$opponents < 1L) return(invisible(NULL))
      current$result <- calculate(current$slots, current)
      snapshot(current)
      result(current$result)
      shiny::showModal(table_holdem_odds_dialog(session$ns, current, cards, language()), session = session)
    }
  })
}
