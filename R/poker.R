#' Rank a five- to seven-card poker hand
#'
#' Evaluate the best five-card combination. Larger scores win; equal scores tie.
#' Ace-low straights rank below six-high straights. Jokers are not supported.
#' @param hand A list of five to seven distinct French cards.
#' @return A numeric score encoding category followed by ordered tie breakers.
#' @export
poker_hand_points <- function(hand) {
  poker_card_ids(hand)
  if (!length(hand) %in% 5:7) stop("A poker hand needs five to seven cards")
  poker_score_values(
    match(vapply(hand, `[[`, "", "value"), c(as.character(2:10), "J", "Q", "K", "A")) + 1L,
    match(vapply(hand, `[[`, "", "suit"), c("Hearts", "Diamonds", "Clubs", "Spades")))
}

poker_card_ids <- function(cards) {
  if (!is.list(cards)) stop("cards must be a list")
  ids <- vapply(cards, function(x) {
    if (!inherits(x, "card") || !x$value %in% c(as.character(2:10), "J", "Q", "K", "A") ||
        !x$suit %in% c("Hearts", "Diamonds", "Clubs", "Spades")) {
      stop("Expected standard French cards without jokers")
    }
    paste(x$value, x$suit, sep = ":")
  }, character(1))
  if (anyDuplicated(ids)) stop("Duplicate cards")
  ids
}

poker_five_score <- function(hand) {
  poker_score_values(
    match(vapply(hand, `[[`, "", "value"), c(as.character(2:10), "J", "Q", "K", "A")) + 1L,
    match(vapply(hand, `[[`, "", "suit"), c("Hearts", "Diamonds", "Clubs", "Spades")))
}

poker_score_values <- function(ranks, suits) {
  encode <- function(category, tie) {
    sum(c(category, tie, rep(0, 5L - length(tie))) * 15^(5:0))
  }
  straight_high <- function(x) {
    unique_ranks <- unique(x)
    if (14L %in% unique_ranks) unique_ranks <- c(unique_ranks, 1L)
    for (high in 14:5) if (all((high - 0:4) %in% unique_ranks)) return(high)
    0L
  }
  counts <- tabulate(ranks, nbins = 14L)
  four <- which(counts == 4L)
  triples <- sort(which(counts >= 3L), decreasing = TRUE)
  pairs <- sort(which(counts >= 2L), decreasing = TRUE)
  flush_suit <- which(tabulate(suits, nbins = 4L) >= 5L)
  if (length(flush_suit)) {
    flush_ranks <- sort(ranks[suits == flush_suit[1L]], decreasing = TRUE)
    high <- straight_high(flush_ranks)
    if (high) return(encode(8, high))
  }
  if (length(four)) return(encode(7, c(four, max(ranks[ranks != four]))))
  if (length(triples) && length(setdiff(pairs, triples[1L]))) {
    return(encode(6, c(triples[1L], max(setdiff(pairs, triples[1L])))))
  }
  if (length(flush_suit)) return(encode(5, head(flush_ranks, 5L)))
  high <- straight_high(ranks)
  if (high) return(encode(4, high))
  if (length(triples)) return(encode(3, c(triples[1L], head(sort(ranks[ranks != triples[1L]], decreasing = TRUE), 2L))))
  if (length(pairs) >= 2L) return(encode(2, c(pairs[1:2], max(ranks[!ranks %in% pairs[1:2]]))))
  if (length(pairs)) return(encode(1, c(pairs, head(sort(ranks[ranks != pairs], decreasing = TRUE), 3L))))
  encode(0, head(sort(ranks, decreasing = TRUE), 5L))
}

check_number <- function(x, name, lower = 0, upper = Inf, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < lower || x > upper || (integer && x != floor(x))) {
    stop(name, " must be a finite ", if (integer) "integer " else "number ",
         "between ", lower, " and ", upper)
  }
  invisible(x)
}

with_poker_seed <- function(seed, fun) {
  if (is.null(seed)) return(fun())
  check_number(seed, "seed", 0, .Machine$integer.max, TRUE)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old, .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  fun()
}

poker_pack <- function() {
  cards <- unlist(lapply(c("Hearts", "Diamonds", "Clubs", "Spades"), function(suit) {
    lapply(c(as.character(2:10), "J", "Q", "K", "A"), card, suit = suit)
  }), recursive = FALSE)
  deck(cards = cards)
}

#' Find the winning players
#' @param game A game with players and optionally a community-card board.
#' @param players Eligible player indices; NULL evaluates all players.
#' @return Original player indices of tied winners.
#' @export
evaluate_poker_hands <- function(game, players = NULL) {
  if (is.null(players)) players <- seq_along(game$players)
  validate_positions(players, length(game$players))
  if (!length(players)) stop("No eligible players")
  scores <- vapply(players, function(i) {
    poker_hand_points(c(as_card_list(game$players[[i]]$hand), game$board))
  }, numeric(1))
  players[scores == max(scores)]
}

#' Award a poker pot
#' @param game A game with players and table pots.
#' @param players Optional eligible player indices, intersected with pot eligibility.
#' @param pot Pot index.
#' @return Updated game, with the selected pot emptied.
#' @export
determine_poker_winner <- function(game, players = NULL, pot = 1) {
  validate_positions(pot, length(game$table$pots))
  if (length(pot) != 1L) stop("Select one pot")
  entry <- game$table$pots[[pot]]
  eligible <- entry$eligible_players
  if (is.null(eligible)) eligible <- seq_along(game$players)
  if (!is.null(players)) eligible <- intersect(eligible, players)
  if (!length(eligible)) stop("No eligible players")
  winners <- if (length(eligible) == 1L) eligible else evaluate_poker_hands(game, eligible)
  check_number(entry$amount, "pot amount")
  for (winner in winners) game$players[[winner]]$points <- game$players[[winner]]$points + entry$amount / length(winners)
  game$table$pots[[pot]]$amount <- 0
  game
}

resolve_poker_player <- function(game, player) {
  if (is.character(player)) player <- which(vapply(game$players, `[[`, "", "name") == player)
  if (is.list(player)) player <- which(vapply(game$players, identical, logical(1), player))
  validate_positions(player, length(game$players))
  if (length(player) != 1L) stop("Select one player")
  player
}

#' Place a legacy poker bet
#' @param game A game with players and table pots.
#' @param player Player index, name, or object.
#' @param bet Nonnegative amount to add to the pot.
#' @param pot Pot index.
#' @return Updated game.
player_bet_poker <- function(game, player, bet, pot = 1) {
  i <- resolve_poker_player(game, player)
  validate_positions(pot, length(game$table$pots))
  if (length(pot) != 1L) stop("Select one pot")
  check_number(bet, "bet", 0, game$players[[i]]$points)
  game$players[[i]]$points <- game$players[[i]]$points - bet
  entry <- game$table$pots[[pot]]
  if (is.null(entry$contributions)) entry$contributions <- rep(0, length(game$players))
  entry$contributions[i] <- entry$contributions[i] + bet
  entry$amount <- entry$amount + bet
  game$table$pots[[pot]] <- entry
  game
}

#' Replace cards in a legacy draw-poker hand
#' @param game A game with a main deck and table discard deck.
#' @param player Player name or index.
#' @param cards Positions to replace, or card objects.
#' @param replacement_deck Optional replacement deck.
#' @param discarding_deck Optional discard deck.
#' @return Updated game. This operation is not part of Texas Hold'em.
player_discard_poker <- function(game, player, cards, replacement_deck = NULL, discarding_deck = NULL) {
  i <- resolve_poker_player(game, player)
  if (is.null(replacement_deck)) replacement_deck <- game$deck
  if (is.null(discarding_deck)) discarding_deck <- game$table$deck
  hand <- game$players[[i]]$hand
  if (is.list(cards)) cards <- which(vapply(hand, function(x) any(vapply(cards, identical, logical(1), x)), logical(1)))
  validate_positions(cards, length(hand))
  n <- length(cards)
  if (n > length(replacement_deck)) stop("Not enough replacement cards")
  game$players[[i]]$hand <- c(as_card_list(hand)[setdiff(seq_along(hand), cards)],
                              as_card_list(replacement_deck)[seq_len(n)])
  game$deck <- discard_card(replacement_deck, seq_len(n))
  game$table$deck <- rebuild_deck(c(as_card_list(discarding_deck), as_card_list(hand)[cards]),
    c(attr(discarding_deck, "optional_cards"), attr(hand, "optional_cards")), attr(discarding_deck, "language"))
  game
}

#' Apply a legacy poker raise, call or fold
#' @param game A game with players and table pots.
#' @param player Player index, name, or object.
#' @param raise NULL folds, zero calls/checks, positive values raise by that amount.
#' @param pot Pot index.
#' @return Updated game. Use the Hold'em engine for full betting and side-pot rules.
player_raise_poker <- function(game, player, raise, pot = 1) {
  i <- resolve_poker_player(game, player)
  validate_positions(pot, length(game$table$pots))
  if (length(pot) != 1L) stop("Select one pot")
  entry <- game$table$pots[[pot]]
  if (is.null(entry$eligible_players)) entry$eligible_players <- seq_along(game$players)
  if (!i %in% entry$eligible_players) stop("Player has folded")
  if (is.null(entry$contributions)) entry$contributions <- rep(0, length(game$players))
  game$table$pots[[pot]] <- entry
  if (is.null(raise)) {
    game$table$pots[[pot]]$eligible_players <- setdiff(entry$eligible_players, i)
    return(game)
  }
  check_number(raise, "raise")
  to_call <- max(entry$contributions) - entry$contributions[i]
  player_bet_poker(game, i, min(to_call + raise, game$players[[i]]$points), pot)
}
