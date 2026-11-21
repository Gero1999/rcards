players <- list(cardgame::player("Alice",points=100))
round <- cardgame::blackjack_game(players,seed=42)
result <- cardgame::play(round,list(cardgame::blackjack_bot()))
stopifnot(result$game$complete)
# In an interactive R console, replace blackjack_bot() with
# console_player(language="it") (or en/es/de).
