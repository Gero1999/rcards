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
#' @param pot Index of the pot to consider (default: 1)
#' @returns A list with player indices of winners and their hand points
#' @export
evaluate_poker_hands <- function(game, players = NULL) {
	if (is.numeric(players)) {
		players <- game$players[players]
	}
	player_points <- sapply(players, function(player) poker_hand_points(player$hand))
	max_points <- max(player_points)
	winners <- which(player_points == max_points)
	winners
}

#' Define a poker winner based on hand points
#' @param game A game object containing players with hands
#' @param players Optional list of player indices to evaluate (default: all players)
#' @param pot Index of the pot to consider (default: 1)
#' @returns Updated game object with winner(s) awarded pot
#' @export
determine_poker_winner <- function(game, players = NULL, pot = 1) {
	
    pot_amount <- game$table$pots[[pot]]$amount
	pot_players <- game$table$pots[[pot]]$eligible_players

	winners <- evaluate_poker_hands(game, players, pot)

	money <- pot_amount / length(winners)
	for (winner in winners) {
		game$players[[winner]]$points <- game$players[[winner]]$points + money
	}
	# Set pot to 0 after distribution
	game$table$pots[[pot]]$amount <- 0
	game
}

#' Define a player bet in poker
#' @param game A game object containing players and table
#' @param player A player object or index
#' @param bet Numeric value of the bet
#' @param pot Index of the pot to place the bet in (default: 1)
#' @returns Updated game object with bet placed in the table pot
player_bet_poker <- function(game, player, bet, pot = 1) {
	# player can be index or object
	if (is.numeric(player)) {
		player_idx <- player
	} else if (is.character(player)) {
		player_idx <- which(sapply(game$players, function(p) p$name == player))
	} else {
		player_idx <- which(sapply(game$players, identical, y = player))
	}
	if (bet > game$players[[player_idx]]$points) {
		stop("Player does not have enough to bet that amount")
	}
	game$players[[player_idx]]$points <- game$players[[player_idx]]$points - bet
	game$table$pots[[pot]]$amount <- game$table$pots[[pot]]$amount + bet
	# Optionally, track how much each player has contributed to each pot
	if (is.null(game$table$pots[[pot]]$contributions)) {
		game$table$pots[[pot]]$contributions <- rep(0, length(game$players))
	}
	game$table$pots[[pot]]$contributions[player_idx] <- game$table$pots[[pot]]$contributions[player_idx] + bet
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


#' Player raise action in poker (handles raise, call, fold, and side pots)
#' @param game A game object containing players and table
#' @param player A player object, name, or index
#' @param raise NULL (fold), 0 (call/check), or numeric (raise amount)
#' @param pot Index of the pot to act on (default: 1)
#' @returns Updated game object with action applied, pots and eligibility updated
player_raise_poker <- function(game, player, raise, pot = 1) {
	# Resolve player index
	if (is.numeric(player)) {
		player_idx <- player
	} else if (is.character(player)) {
		player_idx <- which(sapply(game$players, function(p) p$name == player))
	} else {
		player_idx <- which(sapply(game$players, identical, y = player))
	}

	# Ensure eligible_players exists for this pot
	if (is.null(game$table$pots[[pot]]$eligible_players)) {
		game$table$pots[[pot]]$eligible_players <- which(rep(TRUE, length(game$players)))
	}

	# Ensure contributions exists for this pot
	if (is.null(game$table$pots[[pot]]$contributions)) {
		game$table$pots[[pot]]$contributions <- rep(0, length(game$players))
	}

	# Get current max contribution in this pot
	contribs <- game$table$pots[[pot]]$contributions
	max_contrib <- max(contribs[game$table$pots[[pot]]$eligible_players])
	player_contrib <- contribs[player_idx]

	# FOLD: raise = NULL
	if (is.null(raise)) {
		# Remove player from eligible_players for this and all future pots
		for (p in pot:length(game$table$pots)) {
			if (!is.null(game$table$pots[[p]]$eligible_players)) {
				game$table$pots[[p]]$eligible_players <- setdiff(game$table$pots[[p]]$eligible_players, player_idx)
			}
		}
		return(game)
	}

	# CALL/CHECK: raise == 0
	if (identical(raise, 0)) {
		call_amt <- max_contrib - player_contrib
		if (call_amt > 0) {
			if (call_amt > game$players[[player_idx]]$points) {
				# Player goes all-in, create side pot
				call_amt <- game$players[[player_idx]]$points
			}
			game <- player_bet_poker(game, player_idx, call_amt, pot)
		}
		return(game)
	}

	# RAISE: raise is numeric
	if (is.numeric(raise) && raise > 0) {
		total_to_put <- (max_contrib - player_contrib) + raise
		if (total_to_put > game$players[[player_idx]]$points) {
			# Player can only go all-in
			total_to_put <- game$players[[player_idx]]$points
			raise <- total_to_put - (max_contrib - player_contrib)
		}
		# Place the bet (call + raise)
		game <- player_bet_poker(game, player_idx, total_to_put, pot)

		# Check if player could not match the full raise (all-in/side pot needed)
		if (total_to_put < (max_contrib - player_contrib) + raise) {
			# Create new side pot for the exceeded amount
			exceeded <- ((max_contrib - player_contrib) + raise) - total_to_put
			# New pot index
			new_pot_idx <- length(game$table$pots) + 1
			# Eligible players for new pot: all who matched the full previous bet except this player
			eligible_new <- setdiff(game$table$pots[[pot]]$eligible_players, player_idx)
			game$table$pots[[new_pot_idx]] <- list(
				amount = exceeded * length(eligible_new),
				eligible_players = eligible_new,
				contributions = rep(0, length(game$players))
			)
			# Remove player from eligible_players for new pot
			game$table$pots[[new_pot_idx]]$eligible_players <- setdiff(game$table$pots[[new_pot_idx]]$eligible_players, player_idx)
		}
		return(game)
	}

	stop("Invalid raise value. Must be NULL (fold), 0 (call/check), or positive numeric (raise amount).")
}