table_scopa_capture_picker <- function(view, hand_index, version, selected_capture = NULL) {
  if (!identical(view$game_type, "scopa") || !isTRUE(view$your_turn) ||
      length(hand_index) != 1L || !hand_index %in% seq_along(view$legal$captures)) return(NULL)

  options <- view$legal$captures[[hand_index]]
  cards <- view$center[[1L]]$cards
  language <- view$language
  take_label <- ui("Take %s", language)
  slot <- regexpr("%s", take_label, fixed = TRUE)[[1L]]
  label_prefix <- substr(take_label, 1L, slot - 1L)
  label_suffix <- substring(take_label, slot + 2L)
  labels <- lapply(options, function(take) {
    if (!length(take)) return(ui("leave on table", language))
    taken <- cards[take]
    marks <- lapply(seq_along(taken), function(i) {
      card <- taken[[i]]
      shiny::tagList(if (i > 1L) " + ",
        shiny::tags$span(class = "rc-capture-card", card$value, table_suit_mark(card)))
    })
    names <- vapply(taken, function(card) {
      suffix <- paste0(" ", card$symbol)
      if (endsWith(card$label, suffix)) {
        return(substr(card$label, 1L, nchar(card$label) - nchar(suffix)))
      }
      card$label
    }, "")
    shiny::tagList(
      shiny::tags$span(class = "rc-capture-label", `aria-hidden` = "true", label_prefix, marks, label_suffix),
      shiny::tags$span(class = "sr-only", ui("Take %s", language, paste(names, collapse = " + "))))
  })

  values <- paste(version, hand_index, seq_along(options), sep = ":")
  selected <- if (length(values) == 1L) values else if (length(selected_capture) == 1L &&
    selected_capture %in% values) selected_capture else character()
  shiny::radioButtons("capture", ui("Capture choice", language), choiceNames = labels,
    choiceValues = values, selected = selected)
}
