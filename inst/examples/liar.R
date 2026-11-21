game <- cardgame::liar_game(c("Alice","Bob","Carol"),seed=42)
controllers <- lapply(1:3,function(i) cardgame::liar_bot(seed=i))
result <- cardgame::play(game,controllers,max_actions=10000)
stopifnot(result$game$complete)
# Interactive game: replace one liar_bot() with console_player(language="es").
# Use claim RANK POSITIONS, challenge or pass; q pauses the current turn.
