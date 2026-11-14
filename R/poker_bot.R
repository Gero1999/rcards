#' Create a statistical poker bot
#'
#' Risk is willingness to continue or raise near marginal equity, not skill.
#' Adaptability controls how quickly risk responds to observed opponent aggression;
#' variation adds bounded per-decision noise around that evolving baseline.
#' Reuse the returned bot between decisions or hands to preserve its state.
#' @param risk Baseline risk tolerance, between zero and one.
#' @param adaptability Adjustment rate toward an opponent-dependent risk target.
#' @param variation Maximum random deviation from current risk per decision.
#' @param equity_samples Monte Carlo trials per decision (one to 100000).
#' @param seed Optional seed for reproducible decisions without changing caller RNG.
#' @return A poker_bot object. poker_bot_action() returns its updated state.
#' @export
poker_bot <- function(risk = 0.5, adaptability = 0.2, variation = 0.05,
                      equity_samples = 128L, seed = NULL) {
  for (name in c("risk", "adaptability", "variation")) check_number(get(name), name, 0, 1)
  check_number(equity_samples, "equity_samples", 1, 100000, TRUE)
  if (!is.null(seed)) check_number(seed, "seed", 0, .Machine$integer.max, TRUE)
  structure(list(risk = risk, current_risk = risk, adaptability = adaptability,
    variation = variation, equity_samples = equity_samples, seed = seed,
    decisions = 0L), class = "poker_bot")
}

#' Observe only the acting player's available information
#' @param hand An incomplete holdem_hand state.
#' @return Hole cards of the actor, public board, stacks, contributions, legal
#' actions, and public action history. Opponent cards and the deck are excluded.
#' @export
holdem_observation <- function(hand) {
  legal <- holdem_legal_actions(hand)
  if (hand$complete) stop("Hand is complete")
  i <- legal$actor
  list(actor = i, hole = hand$players[[i]]$hand, board = hand$board,
    stacks = holdem_stacks(hand), contributions = hand$contributions,
    round_bets = hand$round_bets, folded = hand$folded, street = hand$street,
    big_blind = hand$big_blind, pot = sum(hand$contributions),
    contestable_pot = sum(pmin(hand$contributions, hand$contributions[i] + hand$players[[i]]$points)),
    legal = legal, history = hand$history)
}

#' Choose an equity-based poker action
#'
#' Compare random-opponent showdown equity to pot odds, with a risk-dependent
#' margin. This is a heuristic, not a game-theoretic strategy or an opponent
#' hand-range model. Side-pot odds are approximated using contestable contributions.
#' @param bot A poker_bot object.
#' @param observation The result of holdem_observation().
#' @return A list containing action, optional raise-to amount, updated bot,
#' estimated equity, pot odds, and effective risk.
#' @export
poker_bot_action <- function(bot, observation) {
  if (!inherits(bot, "poker_bot")) stop("Expected a poker_bot")
  o <- observation
  if (!is.list(o) || is.null(o$legal$actor) || !identical(o$actor, o$legal$actor)) stop("Invalid observation")
  opponents <- sum(!o$folded) - 1L
  if (opponents < 1L) stop("No opponents")
  recent <- utils::tail(Filter(function(event) event$player != o$actor, o$history), 20L)
  if (length(recent)) {
    aggression <- mean(vapply(recent, function(event) event$action == "raise", logical(1)))
    target <- max(0, min(1, bot$risk + 0.3 * (0.5 - aggression)))
    bot$current_risk <- bot$current_risk + bot$adaptability * (target - bot$current_risk)
  }
  decision_seed <- if (is.null(bot$seed)) NULL else (bot$seed + as.double(bot$decisions)) %% .Machine$integer.max
  choice <- with_poker_seed(decision_seed, function() {
    estimate <- holdem_equity(o$hole, o$board, opponents, bot$equity_samples)
    risk <- max(0, min(1, bot$current_risk + stats::runif(1, -bot$variation, bot$variation)))
    call <- o$legal$to_call
    odds <- if (call > 0) call / (o$contestable_pot + call) else 0
    adjusted <- estimate$equity + 0.2 * (risk - 0.5)
    action <- if ("check" %in% o$legal$actions) "check" else if (adjusted >= odds) "call" else "fold"
    amount <- NULL
    # Raise when estimated equity exceeds the equal-share baseline and pot odds.
    if ("raise" %in% o$legal$actions && adjusted > max(odds + 0.12, 1 / (opponents + 1))) {
      increment <- max(o$big_blind, floor((0.25 + risk) * max(o$pot, o$big_blind)))
      desired <- o$round_bets[o$actor] + call + increment
      amount <- max(o$legal$min_raise_to, min(desired, o$legal$max_raise_to))
      action <- "raise"
    }
    list(action = action, amount = amount, equity = estimate$equity,
         pot_odds = odds, effective_risk = risk)
  })
  bot$decisions <- bot$decisions + 1L
  choice$bot <- bot
  choice
}
