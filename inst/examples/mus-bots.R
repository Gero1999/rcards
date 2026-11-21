# Fully automated play stays silent; inspect the returned structured results.
game <- cardgame::mus_game(c("Alice", "Bob", "Carol", "Dan"), seed = 42)
controllers <- lapply(1:4, function(i) cardgame::mus_bot(equity_samples = 8, seed = i))
result <- cardgame::play(game, controllers, seed = 42)
stopifnot(result$status %in% c("hand_complete", "complete"))
