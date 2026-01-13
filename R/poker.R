## Poker hand points calculation
#' Calculate poker hand points based on standard poker hand rankings
#' @param hand A list of card objects representing the player's hand
#' @returns Numeric value representing the poker hand points
poker_hand_points <- function(hand) {
	# Helper: map card values to ranks
	value_map <- setNames(2:14, c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"))
	values <- sapply(hand, function(card) as.character(card$value))
	suits <- sapply(hand, function(card) card$suit)
	ranks <- sort(value_map[values], decreasing = TRUE)
  
	# Count occurrences
	value_counts <- table(ranks)
	suit_counts <- table(suits)
	is_flush <- any(suit_counts == 5)

    straight_comb <- outer(seq_len(10), 0:4, "+")
	is_straight <- any(apply(straight_comb, 1, function(x) all(x %in% ranks)))
	is_straight <- length(ranks) == length(hand) && (max(ranks) - min(ranks) == length(hand) - 1)
	# Special case: A-2-3-4-5 straight
	if (all(c(14, 2, 3, 4, 5) %in% ranks)) is_straight <- TRUE

	# Poker hand ranking (higher is better)
	# 9: Royal Flush, 8: Straight Flush, 7: Four of a Kind, 6: Full House, 5: Flush, 4: Straight, 3: Three of a Kind, 2: Two Pair, 1: One Pair, 0: High Card
	if (is_flush && is_straight && max(ranks) == 14) {
		base <- 9e6 # Royal Flush
	} else if (is_flush && is_straight) {
		base <- 8e6 # Straight Flush
	} else if (any(value_counts == 4)) {
		base <- 7e6 # Four of a Kind
	} else if (any(value_counts == 3) && any(value_counts == 2)) {
		base <- 6e6 # Full House
	} else if (is_flush) {
		base <- 5e6 # Flush
	} else if (is_straight) {
		base <- 4e6 # Straight
	} else if (any(value_counts == 3)) {
		base <- 3e6 # Three of a Kind
	} else if (sum(value_counts == 2) == 2) {
		base <- 2e6 # Two Pair
	} else if (any(value_counts == 2)) {
		base <- 1e6 # One Pair
	} else {
		base <- 0 # High Card
	}
	# Add tiebreaker: encode ranks as a number
	tiebreak <- sum(ranks * 10^(seq_along(ranks)-1))
	base + tiebreak
}

#' Evaluate poker hands for all players in a game and determine the winner(s)
#' @param game A game object containing players with hands
#' @param players Optional list of player indices to evaluate (default: all players)
#' @returns A list with player indices of winners and their hand points
#' @export
evaluate_poker_hands <- function(game, players = NULL) {
	if (is.null(players)) {
		players <- game$players
	}
	player_points <- sapply(players, function(player) poker_hand_points(player$hand))
	max_points <- max(player_points)
	winners <- which(player_points == max_points)
	players[winners]
}

#' Define a poker winner based on hand points
#' @param game A game object containing players with hands
#' @param players Optional list of player indices to evaluate (default: all players)
#' @returns Updated game object with winner(s) awarded pot
#' @export
determine_poker_winner <- function(game, players = NULL) {
	winners <- evaluate_poker_hands(game, players)
	money <- game$table$pot / length(winners)
	for (winner in winners) {
		winner$points <- winner$points + money
	}
	game
}

#' Define a player bet in poker
#' @param game A game object containing players and table
#' @param player A player object
#' @param bet Numeric value of the bet
#' @returns Updated game object with bet placed in the table pot
player_bet_poker <- function(game, player, bet) {
	if (bet > player$points) {
		stop("Player does not have enough points to bet that amount")
	}
	player$points <- player$points - bet
	game$table$pot <- game$table$pot + bet
	game
}

#' Define a player discard in poker
#' @param game A game object containing players
#' @param player A player name or index
#' @param cards THe player's cards to discard or indices 
#' @param replacement_deck Optional deck to draw replacement cards from (default: main game deck)
#' @param discarding_deck Optional deck to return discarded cards to (default: table deck)
#' @returns Updated game object with player's hand updated with main deck and discarded cards returned to mock deck
player_discard_poker <- function(game, player, cards, replacement_deck = NULL, discarding_deck = NULL) {
	if (is.character(player)) {
		player <- which(sapply(game$players, function(p) p$name == player))
	}
	if (is.null(replacement_deck)) {
		replacement_deck <- game$deck
	}
	if (is.null(discarding_deck)) {
		discarding_deck <- game$table$deck
	}
	
	player_obj <- game$players[[player]]
	
	# Determine indices of cards to discard
	if (is.numeric(cards)) {
		discard_indices <- cards
	} else {
		discard_indices <- which(sapply(player_obj$hand, function(card) any(sapply(cards, function(c) identical(card, c)))))
	}
	
	# Discard cards
	discarded_cards <- player_obj$hand[discard_indices]
	player_obj$hand <- player_obj$hand[-discard_indices]
	
	# Draw replacement cards
	n_replacements <- length(discard_indices)
	new_cards <- replacement_deck[1:n_replacements]
	player_obj$hand <- c(player_obj$hand, new_cards)
	
	# Update decks
	game$deck <- discard_card.deck(replacement_deck, 1:n_replacements)
	game$table$deck <- deck(c(unclass(discarding_deck), discarded_cards), 
	                        optional_cards = attr(discarding_deck, "optional_cards"), 
	                        language = attr(discarding_deck, "language"))
	
	game$players[[player]] <- player_obj
	game
}
