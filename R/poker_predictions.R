#' Functions to make predictions on poker hands
hand_chances_poker <- function(hand, deck = deck(), n_players = 2, n = 1000) {
    # Create n alternative hands using left cards in the deck
    deck_remaining <- remove_card.deck(deck, hand)
    win_count <- list(win = 0, tie = 0, loss = 0)
    for (i in 1:n) {
        # Define other players' hands
        other_hands_points <- list()
        deck_remaining_copy <- deck_remaining
        for (p in 2:n_players) {
            other_hands_points[[p - 1]] <- poker_hand_points(sample(deck_remaining_copy, 5))
            deck_remaining_copy <- remove_card.deck(deck_remaining_copy, other_hands[[p - 1]])
        }
        best_enemy_points <- max(unlist(other_hands_points))
        player_points <- poker_hand_points(hand)
        if (player_points > best_enemy_points) {
            win_count$win <- win_count$win + 1/n
        } else if (player_points == best_enemy_points) {
            win_count$tie <- win_count$tie + 1/n
        } else {
            win_count$loss <- win_count$loss + 1/n
        }
    }
    win_count
}
