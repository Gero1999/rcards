#' Request a controller decision
#'
#' Controllers receive only an observation, not full game state. Methods return
#' list(decision = list(action = ..., amount = ..., cards = ...), controller = ...).
#' The updated controller retains private decision-making state. Plain functions
#' accept one observation and return a decision directly. Stateful policies use
#' policy_player(). The poker_bot method supports Hold'em observations only.
#' @param controller A console player, poker bot, policy player, or policy function.
#' @param observation A seat-specific result from game_observation().
#' @return A decision and updated controller.
#' @export
choose_action <- function(controller, observation) UseMethod("choose_action")

#' @export
choose_action.default <- function(controller, observation) stop("Unsupported game controller")

#' @export
choose_action.function <- function(controller, observation) {
  list(decision = validate_game_decision(controller(observation)), controller = controller)
}

#' @export
choose_action.poker_bot <- function(controller, observation) {
  if (!identical(observation$game_type, "holdem")) stop("Poker bots support Hold'em only")
  choice <- poker_bot_action(controller, observation)
  decision <- list(action = choice$action)
  if (!is.null(choice$amount)) decision$amount <- choice$amount
  list(decision = decision, controller = choice$bot)
}

#' Create a stateful scripted player
#' @param policy Function of observation and state, returning a list with decision
#' (an action list) and state (the updated private state).
#' @param state Initial private controller state.
#' @return A game_controller usable by play() or simulate_poker_game().
#' @export
policy_player <- function(policy, state = NULL) {
  if (!is.function(policy)) stop("policy must be a function")
  structure(list(policy = policy, state = state), class = c("policy_player", "game_controller"))
}

#' @export
choose_action.policy_player <- function(controller, observation) {
  choice <- controller$policy(observation, controller$state)
  if (!is.list(choice) || !all(c("decision", "state") %in% names(choice))) {
    stop("Stateful policy must return decision and state")
  }
  decision <- validate_game_decision(choice$decision)
  controller$state <- choice$state
  list(decision = decision, controller = controller)
}

is_game_controller <- function(x) {
  is.function(x) || inherits(x, "game_controller") || inherits(x, "poker_bot")
}
