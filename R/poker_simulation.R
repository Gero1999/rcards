#' Simulate complete individual no-limit Hold'em hands
#'
#' Every hand starts with the supplied stacks; chips do not carry between hands.
#' Bot states do carry over, allowing risk adaptation across the simulation.
#' No GUI or interactive console input is required. Custom policies receive only
#' the acting player's observation and return action and optional amount.
#' @param players A list of funded player objects.
#' @param bots One controller per seat: poker_bot(), policy_player(), or a policy
#' function. Simulation uses the shared play() loop and retains controller state.
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
  if (!is.list(bots) || length(bots) != n || !all(vapply(bots, is_game_controller, logical(1)))) {
    stop("Provide one supported controller per seat")
  }
  with_poker_seed(seed, function() {
    completed <- vector("list", hands)
    net <- matrix(0, nrow = hands, ncol = n)
    for (j in seq_len(hands)) {
      button <- ((dealer + j - 2L) %% n) + 1L
      h <- holdem_hand(players, button, small_blind, big_blind)
      played <- play(h, bots, max_actions = max_actions)
      if (played$status == "action_limit") stop("Hand exceeded max_actions")
      if (!identical(played$status, "complete")) stop("Simulation stopped: ", played$status)
      h <- played$game
      bots <- played$controllers
      completed[[j]] <- h
      net[j,] <- holdem_stacks(h) - vapply(players, `[[`, 0, "points")
    }
    colnames(net) <- vapply(players, `[[`, "", "name")
    list(hands = completed, bots = bots, net_chips = net)
  })
}
