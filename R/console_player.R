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
#' @return A game_controller. Use one controller per seat in play(). Multiple
#' humans can share a terminal, but previously displayed hands remain visible.
#' @export
console_player <- function(read = NULL, write = NULL, language = NULL) {
  if (!is.null(read) && !is.function(read)) stop("read must be a function")
  if (is.null(write)) write <- function(text) cat(text, "\n", sep = "")
  if (!is.function(write)) stop("write must be a function")
  if (!is.null(language)) translate_card(card("A", "Spades"), language)
  structure(list(read = read, write = write, language = language),
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

console_card_text <- function(cards, language = NULL, numbered = FALSE) {
  if (!length(cards)) return("(none)")
  text <- vapply(cards, function(x) {
    if (!is.null(language)) x <- translate_card(x, language)
    x$display
  }, character(1))
  if (numbered) text <- paste0(seq_along(text), ") ", text)
  paste(text, collapse = " | ")
}

console_observation_text <- function(o, language) {
  actor <- o$actor
  title <- if (o$game_type == "holdem") "Texas Hold'em" else "Mus"
  text <- c(paste0("\n", title, " - ", o$player_names[actor], " (seat ", actor, ")"),
    paste0("Your cards: ", console_card_text(o$hole, language, TRUE)))
  if (o$game_type == "holdem") {
    text <- c(text, paste0("Street: ", o$street), paste0("Board: ", console_card_text(o$board, language)),
      paste0("Stacks: ", paste(paste0(o$player_names, "=", o$stacks), collapse = "; ")),
      paste0("Pot: ", o$pot, " | To call: ", o$legal$to_call))
    if ("raise" %in% o$legal$actions) text <- c(text,
      paste0("Raise-to bounds (total street bet): ", o$legal$min_raise_to, " to ", o$legal$max_raise_to))
  } else {
    partner <- which(o$teams == o$teams[actor] & seq_along(o$teams) != actor)
    text <- c(text, paste0("Hand ", o$hand_number, " | Phase: ", o$phase,
      " | Mano: ", o$player_names[o$mano]),
      paste0("Your partner: ", o$player_names[partner]),
      paste0("Scores: team 1 (seats 1/3)=", o$scores[1L], "; team 2 (seats 2/4)=", o$scores[2L]),
      paste0("Rules: ", o$rules$kings, " kings; target ", o$rules$points_to_win))
    if (!is.null(o$wager)) text <- c(text, paste0("Current stake: ", if (o$ordago) "ordago" else o$wager))
    if (!is.null(o$legal$min_stake)) text <- c(text, paste0("Minimum total stake: ", o$legal$min_stake))
  }
  if (length(o$history)) {
    recent <- vapply(utils::tail(o$history, 8L), function(event) {
      detail <- if (!is.null(event$amount)) paste0(" ", event$amount) else ""
      if (!is.null(event$discard_count)) detail <- paste0(" ", event$discard_count, " card(s)")
      paste0(o$player_names[event$player], ": ", event$action, detail)
    }, character(1))
    text <- c(text, "Recent public actions:", recent)
  }
  c(text, paste0("Actions: ", paste(paste0(seq_along(o$legal$actions), ") ", o$legal$actions), collapse = " | ")),
    "Use an action name or number; raise/bet TOTAL; discard POSITIONS (empty to keep all); q to quit.")
}

#' @export
choose_action.console_player <- function(controller, observation) {
  controller$write(paste(console_observation_text(observation, controller$language), collapse = "\n"))
  line <- console_read(controller, "Your action: ")
  decision <- console_decision(line, observation$legal, controller)
  list(decision = decision, controller = controller)
}
