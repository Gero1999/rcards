#' Simulate complete individual no-limit Hold'em hands
#'
#' Every hand starts with the supplied stacks; chips do not carry between hands.
#' Bot states do carry over, allowing risk adaptation across the simulation.
#' No GUI or interactive console input is required. Custom policies receive only
#' the acting player's observation and return action and optional amount.
#' @param players A list of funded player objects.
#' @param bots One poker_bot per seat, or one policy function per seat.
#' @param hands Number of independent hands to simulate (one to 10000).
#' @param dealer First hand's dealer seat; rotates each hand.
#' @param small_blind,big_blind Integer blinds.
#' @param seed Optional reproducible seed, preserving caller RNG state.
#' @param max_actions Per-hand action limit; errors instead of silently truncating.
#' @return A list of completed hand states, updated bots, and per-hand net chip
#' results. Each result row sums to zero.
#' @export
simulate_poker_game <- function(players, bots = NULL, hands = 1L, dealer = 1L,
    small_blind = 1L, big_blind = 2L, seed = NULL, max_actions = 1000L) {
  check_number(hands, "hands", 1, 10000, TRUE)
  check_number(max_actions, "max_actions", 1, 100000, TRUE)
  n <- length(players)
  if (!is.list(players) || !n %in% 2:10 ||
      !all(vapply(players, inherits, logical(1), "player"))) stop("Expected two to ten players")
  check_number(dealer, "dealer", 1, n, TRUE)
  if (is.null(bots)) bots <- lapply(seq_len(n), function(i) poker_bot())
  if (length(bots) != n || !all(vapply(bots, function(x) inherits(x,"poker_bot") || is.function(x), logical(1)))) {
    stop("Provide one bot or policy function per seat")
  }
  with_poker_seed(seed, function() {
    completed <- vector("list", hands)
    net <- matrix(0, nrow = hands, ncol = n)
    for (j in seq_len(hands)) {
      button <- ((dealer + j - 2L) %% n) + 1L
      h <- holdem_hand(players, button, small_blind, big_blind)
      actions <- 0L
      while (!h$complete) {
        if (actions >= max_actions) stop("Hand exceeded max_actions")
        i <- h$actor
        observation <- holdem_observation(h)
        if (is.function(bots[[i]])) choice <- bots[[i]](observation) else {
          choice <- poker_bot_action(bots[[i]], observation)
          bots[[i]] <- choice$bot
        }
        if (!is.list(choice) || is.null(choice$action)) stop("Policy must return an action list")
        h <- holdem_action(h, choice$action, choice$amount)
        actions <- actions + 1L
      }
      completed[[j]] <- h
      net[j,] <- holdem_stacks(h) - vapply(players, `[[`, 0, "points")
    }
    colnames(net) <- vapply(players, `[[`, "", "name")
    list(hands = completed, bots = bots, net_chips = net)
  })
}
