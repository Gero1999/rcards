#' Play a game with human, bot, or scripted controllers
#'
#' Each seat has a controller, separate from the player object. Controllers see
#' only game_observation() and return decisions to game_action(), so interactive
#' play and simulation use the same validation and rule engines.
#'
#' By default stop after one hand. until = "game" automatically advances Mus
#' hands until a partnership reaches the score target; Hold'em still represents
#' one hand. Stateful controllers are returned and can be reused in later calls.
#'
#' Invalid console answers are retried without changing game state. Invalid
#' scripted or bot decisions raise errors. Quitting, EOF, or interruption during
#' decision input returns the current state. Resume with play(result$game,
#' result$controllers). Action/input limits also return an unfinished state.
#' @param game A playable game object.
#' @param controllers One controller per seat: console_player(), poker_bot(),
#' policy_player(), or a function of observation returning an action list.
#' @param until Stop at hand completion (hand) or entire-game completion (game).
#' @param max_actions Maximum accepted actions in this call.
#' @param max_invalid Maximum consecutive invalid console answers for one turn.
#' @param seed Optional seed for this call, preserving the caller's RNG state.
#' @return A game_play object with game, controllers, status, actions, result
#' (a public summary), and last_error when an input limit was reached. Status is
#' complete, hand_complete, quit, interrupted, action_limit, or input_limit.
#' @export
play <- function(game, controllers, until = c("hand", "game"),
                 max_actions = 10000L, max_invalid = 20L, seed = NULL) {
  until <- match.arg(until)
  check_number(max_actions, "max_actions", 1, 1000000, TRUE)
  check_number(max_invalid, "max_invalid", 1, 1000000, TRUE)
  if (!inherits(game, "game")) stop("Expected a game")
  # Establish the game's protocol before collecting any input.
  game_complete(game); game_hand_complete(game)
  if (!is.list(controllers) || length(controllers) != length(game$players) ||
      !all(vapply(controllers, is_game_controller, logical(1)))) stop("Provide one supported controller per seat")
  result <- with_poker_seed(seed, function() {
    actions <- 0L; invalid <- 0L; last_error <- NULL
    finish <- function(status) {
      structure(list(game = game, controllers = controllers, status = status,
        actions = actions, result = game_result(game),
        last_error = if (status == "input_limit") last_error else NULL,
        language = {human <- Filter(function(x) inherits(x,"console_player"),controllers); if(length(human)) human[[1]]$language else NULL}), class = "game_play")
    }
    repeat {
      if (game_complete(game)) return(finish("complete"))
      if (game_hand_complete(game)) {
        if (until == "hand") return(finish("hand_complete"))
        console_notify(game, controllers, "hand_complete")
        game <- game_next_hand(game)
      }
      if (actions >= max_actions) return(finish("action_limit"))
      legal <- game_legal_actions(game)
      i <- legal$actor
      check_number(i, "actor", 1, length(controllers), TRUE)
      controller <- controllers[[i]]
      human <- inherits(controller, "console_player")
      choice <- tryCatch(choose_action(controller, game_observation(game)),
        console_input_error = function(e) e,
        interrupt = function(e) list(decision = list(action = "quit"),
          controller = controller, interrupted = TRUE))
      if (inherits(choice, "console_input_error")) {
        if (!human) stop(choice)
        error <- choice
      } else {
        if (!is.list(choice) || !all(c("decision", "controller") %in% names(choice)) ||
            !is_game_controller(choice$controller)) stop("Controller must return decision and an updated controller")
        decision <- validate_game_decision(choice$decision)
        if (identical(decision$action, "quit")) {
          controllers[[i]] <- choice$controller
          return(finish(if (isTRUE(choice$interrupted)) "interrupted" else "quit"))
        }
        updated <- tryCatch(game_action(game, decision), error = function(e) e)
        if (!inherits(updated, "error")) {
          game <- updated; controllers[[i]] <- choice$controller
          actions <- actions + 1L; invalid <- 0L; last_error <- NULL
          next
        }
        if (!human) stop(updated)
        error <- updated
      }
      invalid <- invalid + 1L; last_error <- conditionMessage(error)
      controller$write(ui("Please try again: %s", controller$language, console_error(last_error, controller$language)))
      if (invalid >= max_invalid) return(finish("input_limit"))
    }
  })
  console_notify(result$game, result$controllers, result$status)
  result
}

game_summary_text <- function(result, language = NULL) {
  if (identical(result$game_type, "holdem")) {
    text <- c(ui("Hold'em | Phase: %s",language,ui(toupper(result$phase),language)),
      ui("Stacks: %s",language,paste(paste0(result$player_names, "=", result$stacks), collapse = "; ")))
    if (result$complete) text <- c(text,
      ui("Payouts: %s",language,paste(paste0(result$player_names, "=", result$payouts), collapse = "; ")))
    text
  } else if (identical(result$game_type, "mus")) {
    text <- c(ui("Mus | Hand %s | Phase: %s",language,result$hand_number,ui(toupper(result$phase),language)),
      ui("Scores: team 1=%s; team 2=%s",language,result$scores[1L],result$scores[2L]))
    if (result$complete) text <- c(text, ui("Winning partnership: team %s",language,result$winner))
    text
  } else {
    paste0("Game complete: ", isTRUE(result$complete))
  }
}

game_play_text <- function(x) {
  c(ui("Play status: %s | Accepted actions: %s", x$language, ui(x$status,x$language), x$actions), game_summary_text(x$result, x$language))
}

#' Print a play result without revealing private cards
#' @param x A game_play object.
#' @param ... Unused arguments.
#' @return x invisibly.
#' @export
print.game_play <- function(x, ...) {
  cat(paste(game_play_text(x), collapse = "\n"), "\n", sep = "")
  invisible(x)
}
