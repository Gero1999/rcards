#' Create a human console player
#'
#' Enter an action name or menu number. Raises/bets accept a total amount, such
#' as "raise 10"; discards accept positions, such as "discard 1 3". A bare
#' "discard" stands pat. "q", "quit", or "exit" leaves the game unchanged at
#' the current turn. EOF and keyboard interruption also return a resumable game.
#' @param read Optional function of prompt returning one input line. The default
#' uses readline() in interactive R; inject a function for scripted tests or UIs.
#' @param write Optional function of text. Defaults to cat(text, "\\n").
#' @param language Card-display language, en or es; NULL retains existing displays.
#' @param style Card borders: auto selects Unicode on UTF-8 terminals, or ascii.
#' @param width Maximum card-row width (16 to 240 columns).
#' @return A game_controller. Use one controller per seat in play(). Multiple
#' humans can share a terminal, but previously displayed hands remain visible.
#' @export
console_player <- function(read = NULL, write = NULL, language = NULL,
    style = c("auto", "unicode", "ascii"), width = getOption("width", 80L)) {
  style <- match.arg(style)
  check_number(width, "width", 16, 240, TRUE)
  if (!is.null(read) && !is.function(read)) stop("read must be a function")
  if (is.null(write)) write <- console_default_write
  if (!is.function(write)) stop("write must be a function")
  if (!is.null(language)) translate_card(card("A", "Spades"), language)
  structure(list(read = read, write = write, language = language, style = style, width = width),
            class = c("console_player", "game_controller"))
}

console_input_error <- function(message) {
  stop(structure(list(message = message, call = NULL),
    class = c("console_input_error", "error", "condition")))
}

console_read <- function(controller, prompt) {
  if (is.null(controller$read)) {
    if (!interactive()) stop("Console input needs an interactive R session or a custom read function")
    readline(prompt)
  } else controller$read(prompt)
}

console_number <- function(value) {
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || is.na(number) || !is.finite(number)) console_input_error("Enter a finite number")
  number
}

console_decision <- function(line, legal, controller) {
  if (!length(line) || (length(line) == 1L && is.na(line))) return(list(action = "quit"))
  if (!is.character(line) || length(line) != 1L) console_input_error("Enter one line of text")
  tokens <- strsplit(tolower(trimws(line)), "[[:space:],]+")[[1L]]
  if (!length(tokens) || !nzchar(tokens[1L])) console_input_error("Choose an action, or q to quit")
  action <- tokens[1L]
  if (action %in% c("q", "quit", "exit")) return(list(action = "quit"))
  if (grepl("^[0-9]+$", action)) {
    index <- suppressWarnings(as.numeric(action))
    if (!is.finite(index) || index < 1 || index > length(legal$actions)) console_input_error("Choose a listed menu number")
    action <- legal$actions[index]
  }
  if (!action %in% legal$actions) console_input_error("Choose one of the listed legal actions")
  arguments <- tokens[-1L]
  if (action %in% c("raise", "bet")) {
    if (!length(arguments)) {
      value <- console_read(controller, "Total amount (or q to quit): ")
      if (!length(value) || (length(value) == 1L && (is.na(value) || tolower(trimws(value)) %in% c("q","quit","exit")))) {
        return(list(action = "quit"))
      }
      arguments <- value
    }
    if (length(arguments) != 1L) console_input_error("Provide one total amount")
    return(list(action = action, amount = console_number(arguments)))
  }
  if (action == "discard") {
    if (!length(arguments) || identical(arguments, "none") || identical(arguments, "0")) positions <- integer() else {
      positions <- unname(vapply(arguments, console_number, numeric(1)))
    }
    return(list(action = action, cards = positions))
  }
  if (length(arguments)) console_input_error("This action takes no amount or positions")
  list(action = action)
}

#' @export
choose_action.console_player <- function(controller, observation) {
  controller$write(paste(console_observation_text(observation, controller), collapse = "\n"))
  line <- console_read(controller, "Your action: ")
  decision <- console_decision(line, observation$legal, controller)
  list(decision = decision, controller = controller)
}
