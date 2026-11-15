players <- list(rcards::player("Alice",points=100))
round <- rcards::blackjack_game(players,seed=42)
result <- rcards::play(round,list(rcards::blackjack_bot()))
stopifnot(result$game$complete)
# In an interactive R console, replace blackjack_bot() with
# console_player(language="it") (or en/es/de).
