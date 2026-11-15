# Fully automated play stays silent; inspect the returned structured results.
game <- rcards::mus_game(c("Alice", "Bob", "Carol", "Dan"), seed = 42)
controllers <- lapply(1:4, function(i) rcards::mus_bot(equity_samples = 8, seed = i))
result <- rcards::play(game, controllers, seed = 42)
stopifnot(result$status %in% c("hand_complete", "complete"))
