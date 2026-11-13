#' Estimate Hold'em showdown equity
#'
#' Sample unknown opponent hole cards and remaining board cards without replacement.
#' Opponents have uniform random holdings; betting history is not a hand-range model.
#' Equity counts a tied win as the fraction of the pot won. It does not account
#' for side-pot amounts or future betting. Sampling is bounded by equity_samples.
#' @param hole Two known French cards.
#' @param board Zero, three, four, or five known community cards.
#' @param opponents Number of opponents (one to nine).
#' @param equity_samples Number of Monte Carlo trials (one to 100000).
#' @param seed Optional seed, preserving the caller's RNG state.
#' @return A list of equity, win/tie/loss frequencies, sample count, and standard error.
#' @export
holdem_equity <- function(hole, board = list(), opponents = 1L,
                          equity_samples = 256L, seed = NULL) {
  if (length(hole) != 2L || !length(board) %in% c(0L, 3:5)) stop("Expected two hole cards and a legal board")
  check_number(opponents, "opponents", 1, 9, TRUE)
  check_number(equity_samples, "equity_samples", 1, 100000, TRUE)
  known <- poker_card_ids(c(as_card_list(hole), as_card_list(board)))
  pack <- poker_pack()
  ids <- poker_card_ids(pack)
  available <- which(!ids %in% known)
  rank <- match(vapply(pack, `[[`, "", "value"), c(as.character(2:10), "J", "Q", "K", "A")) + 1L
  suit <- match(vapply(pack, `[[`, "", "suit"), c("Hearts", "Diamonds", "Clubs", "Spades"))
  hero <- match(poker_card_ids(hole), ids)
  fixed_board <- match(poker_card_ids(board), ids)
  needed <- 5L - length(board) + 2L * opponents
  if (needed > length(available)) stop("Not enough unknown cards")
  trials <- with_poker_seed(seed, function() {
    shares <- numeric(equity_samples); outcomes <- integer(equity_samples)
    for (i in seq_len(equity_samples)) {
      draw <- available[sample.int(length(available), needed)]
      community <- c(fixed_board, draw[seq_len(5L - length(board))])
      rest <- draw[-seq_len(5L - length(board))]
      if (length(board) == 5L) rest <- draw
      hero_score <- poker_score_values(rank[c(hero, community)], suit[c(hero, community)])
      enemy <- vapply(seq_len(opponents), function(j) {
        hand <- c(rest[c(2L * j - 1L, 2L * j)], community)
        poker_score_values(rank[hand], suit[hand])
      }, numeric(1))
      best <- max(c(hero_score, enemy))
      if (hero_score == best) {
        ties <- sum(enemy == best)
        shares[i] <- 1 / (1L + ties)
        outcomes[i] <- if (ties) 1L else 2L
      }
    }
    list(shares = shares, outcomes = outcomes)
  })
  list(equity = mean(trials$shares), win = mean(trials$outcomes == 2L),
       tie = mean(trials$outcomes == 1L), loss = mean(trials$outcomes == 0L),
       samples = equity_samples,
       standard_error = if (equity_samples > 1L) stats::sd(trials$shares) / sqrt(equity_samples) else NA_real_)
}

#' Estimate random-opponent five-card draw outcomes
#' @param hand Five known French cards.
#' @param deck A deck containing the known hand and available cards.
#' @param n_players Total players, including the known hand.
#' @param n Number of trials.
#' @return Win, tie, and loss frequencies. For Hold'em use holdem_equity().
#' @export
hand_chances_poker <- function(hand, deck = poker_pack(), n_players = 2, n = 1000) {
  if (length(hand) != 5L) stop("Expected five cards")
  known <- poker_card_ids(hand); ids <- poker_card_ids(deck)
  check_number(n_players, "n_players", 2, 10, TRUE)
  check_number(n, "n", 1, 100000, TRUE)
  if (!all(known %in% ids)) stop("deck must contain the known hand")
  remaining <- as_card_list(deck)[!ids %in% known]
  if (length(remaining) < 5L * (n_players - 1L)) stop("Not enough opponent cards")
  score <- poker_hand_points(hand)
  outcomes <- integer(n)
  for (i in seq_len(n)) {
    selected <- sample.int(length(remaining), 5L * (n_players - 1L))
    enemies <- vapply(seq_len(n_players - 1L), function(j) {
      poker_hand_points(remaining[selected[seq.int(5L*j-4L, 5L*j)]])
    }, numeric(1))
    outcomes[i] <- if (score > max(enemies)) 2L else if (score == max(enemies)) 1L else 0L
  }
  list(win = mean(outcomes == 2L), tie = mean(outcomes == 1L), loss = mean(outcomes == 0L))
}
