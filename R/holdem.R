#' Start a no-limit Texas Hold'em hand
#'
#' Seats run clockwise. The dealer posts the small blind in heads-up play.
#' Chips and wagers are integer units. Cards are shuffled unless pack is supplied;
#' a supplied pack is dealt from the top, two rounds clockwise after the dealer.
#' @param players Two to ten player objects with positive integer points (stacks).
#' @param dealer Dealer seat index.
#' @param small_blind,big_blind Positive integer blind amounts.
#' @param seed Optional random seed, preserving the caller's RNG state.
#' @param pack Optional complete 52-card French deck, already ordered.
#' @return A holdem_hand state; use holdem_action() until complete is TRUE.
#' @export
holdem_hand <- function(players, dealer = 1L, small_blind = 1L,
                        big_blind = 2L, seed = NULL, pack = NULL) {
  if (!is.list(players) || !length(players) %in% 2:10 ||
      !all(vapply(players, inherits, logical(1), "player"))) stop("Expected two to ten players")
  n <- length(players)
  check_number(dealer, "dealer", 1, n, TRUE)
  check_number(small_blind, "small_blind", 1, integer = TRUE)
  check_number(big_blind, "big_blind", small_blind, integer = TRUE)
  for (p in players) check_number(p$points, "stack", 1, integer = TRUE)
  if (is.null(pack)) pack <- with_poker_seed(seed, function() shuffle(poker_pack()))
  ids <- poker_card_ids(pack)
  if (length(ids) != 52L) stop("pack must contain 52 distinct French cards")
  pack <- deck(cards = as_card_list(pack))
  h <- structure(list(players = players, dealer = as.integer(dealer),
    small_blind = small_blind, big_blind = big_blind, deck = pack,
    board = list(), burned = list(), street = "preflop", round_bets = rep(0, n),
    contributions = rep(0, n), folded = rep(FALSE, n), current_bet = big_blind,
    min_raise = big_blind, acted_at = rep(NA_real_, n), pending = seq_len(n),
    actor = NULL, complete = FALSE, pots = list(), payouts = rep(0, n),
    history = list(), initial_chips = sum(vapply(players, `[[`, 0, "points"))),
    class = "holdem_hand")
  for (i in seq_len(n)) h$players[[i]]$hand <- list()
  order <- clockwise_seats(n, dealer)
  for (round in 1:2) for (i in order) {
    h$players[[i]]$hand <- c(h$players[[i]]$hand, as_card_list(h$deck[1L]))
    h$deck <- h$deck[-1L]
  }
  sb <- if (n == 2L) dealer else order[1L]
  bb <- clockwise_seats(n, sb)[1L]
  h <- holdem_pay(h, sb, min(small_blind, h$players[[sb]]$points))
  h <- holdem_pay(h, bb, min(big_blind, h$players[[bb]]$points))
  # A blind that cannot cover its nominal amount remains the bring-in for players
  # with chips, including an opponent whose full blind is an uncalled excess.
  h <- holdem_progress(h, bb)
  h
}

clockwise_seats <- function(n, after) ((seq_len(n) + after - 1L) %% n) + 1L

holdem_stacks <- function(hand) vapply(hand$players, `[[`, 0, "points")

holdem_pay <- function(h, i, amount) {
  h$players[[i]]$points <- h$players[[i]]$points - amount
  h$round_bets[i] <- h$round_bets[i] + amount
  h$contributions[i] <- h$contributions[i] + amount
  h
}

#' Inspect legal actions for the current Hold'em player
#' @param hand A holdem_hand state.
#' @return A list with actor, actions, call amount, and inclusive raise-to bounds.
#' A short all-in does not reopen betting unless cumulative short raises reach
#' a full raise since the player's last action. Raises specify total street bets.
#' @export
holdem_legal_actions <- function(hand) {
  if (!inherits(hand, "holdem_hand")) stop("Expected a holdem_hand")
  if (hand$complete) return(list(actor = NULL, actions = character()))
  i <- hand$actor
  stack <- hand$players[[i]]$points
  call <- max(0, hand$current_bet - hand$round_bets[i])
  max_to <- hand$round_bets[i] + stack
  reopened <- is.na(hand$acted_at[i]) || hand$current_bet - hand$acted_at[i] >= hand$min_raise
  opposition <- any(!hand$folded[-i] & holdem_stacks(hand)[-i] > 0)
  can_raise <- max_to > hand$current_bet && reopened && opposition
  min_to <- hand$current_bet + hand$min_raise
  if (hand$current_bet == 0) min_to <- hand$big_blind
  actions <- c("fold", if (call == 0) "check" else "call", if (can_raise) "raise")
  list(actor = i, actions = actions, to_call = min(call, stack),
       min_raise_to = if (can_raise) min(min_to, max_to) else NA_real_,
       max_raise_to = if (can_raise) max_to else NA_real_)
}

#' Apply a legal Hold'em action
#' @param hand A holdem_hand state.
#' @param action One of fold, check, call, raise.
#' @param amount For a raise, the player's total street contribution (raise-to).
#' @return Updated state, automatically advancing streets and settling the hand.
#' @export
holdem_action <- function(hand, action, amount = NULL) {
  legal <- holdem_legal_actions(hand)
  if (length(action) != 1L || is.na(action) || !action %in% legal$actions) stop("Illegal action")
  if (action != "raise" && !is.null(amount)) stop("Only raises take an amount")
  h <- hand; i <- legal$actor
  old_bet <- h$current_bet
  if (action == "fold") h$folded[i] <- TRUE
  if (action == "call") h <- holdem_pay(h, i, legal$to_call)
  if (action == "raise") {
    check_number(amount, "raise-to", legal$min_raise_to, legal$max_raise_to, TRUE)
    h <- holdem_pay(h, i, amount - h$round_bets[i])
    increase <- amount - old_bet
    if (increase >= h$min_raise) h$min_raise <- increase
    h$current_bet <- amount
  }
  h$acted_at[i] <- h$current_bet
  h$pending <- setdiff(h$pending, i)
  if (action == "raise") {
    owes <- which(!h$folded & holdem_stacks(h) > 0 & h$round_bets < h$current_bet)
    h$pending <- union(h$pending, owes)
  }
  h$history[[length(h$history) + 1L]] <- list(street = h$street, player = i,
    action = action, amount = amount, to_call = legal$to_call)
  holdem_progress(h, i)
}

holdem_progress <- function(h, after) {
  repeat {
    live <- which(!h$folded)
    if (length(live) == 1L) return(holdem_finish(h))
    active <- which(!h$folded & holdem_stacks(h) > 0)
    h$pending <- intersect(h$pending, active)
    # A lone player with chips need only resolve an outstanding call.
    if (length(active) <= 1L) {
      h$pending <- active[h$round_bets[active] < h$current_bet]
    }
    if (length(h$pending)) {
      h$actor <- clockwise_seats(length(h$players), after)[
        clockwise_seats(length(h$players), after) %in% h$pending][1L]
      return(h)
    }
    if (h$street == "river") return(holdem_finish(h))
    h$burned <- c(h$burned, as_card_list(h$deck[1L])); h$deck <- h$deck[-1L]
    count <- if (h$street == "preflop") 3L else 1L
    h$board <- c(h$board, as_card_list(h$deck[seq_len(count)]))
    h$deck <- h$deck[-seq_len(count)]
    h$street <- switch(h$street, preflop = "flop", flop = "turn", turn = "river")
    h$round_bets[] <- 0; h$current_bet <- 0
    h$min_raise <- h$big_blind; h$acted_at[] <- NA_real_
    h$pending <- active; after <- h$dealer
  }
}

holdem_finish <- function(h) {
  levels <- sort(unique(h$contributions[h$contributions > 0]))
  previous <- 0
  h$pots <- list(); h$payouts[] <- 0
  for (level in levels) {
    contributors <- which(h$contributions >= level)
    amount <- (level - previous) * length(contributors)
    eligible <- contributors[!h$folded[contributors]]
    # A single contributor receives its uncalled excess, including after folding.
    if (length(contributors) == 1L) winners <- contributors else {
      if (!length(eligible)) stop("Invalid pot without eligible players")
      winners <- if (length(eligible) == 1L) eligible else evaluate_poker_hands(h, eligible)
    }
    share <- amount %/% length(winners)
    remainder <- amount %% length(winners)
    h$payouts[winners] <- h$payouts[winners] + share
    # Odd chips go to tied winners clockwise from the button.
    priority <- clockwise_seats(length(h$players), h$dealer)
    odd <- priority[priority %in% winners][seq_len(remainder)]
    if (length(odd)) h$payouts[odd] <- h$payouts[odd] + 1
    h$pots[[length(h$pots) + 1L]] <- list(amount = amount,
      eligible_players = eligible, winners = winners, uncalled = length(contributors) == 1L)
    previous <- level
  }
  for (i in seq_along(h$players)) h$players[[i]]$points <- h$players[[i]]$points + h$payouts[i]
  h$actor <- NULL; h$pending <- integer(); h$complete <- TRUE
  h$street <- "complete"
  if (sum(holdem_stacks(h)) != h$initial_chips) stop("Chip conservation failed")
  h
}
