players <- list(cardgame::player("Alice", points = 100),
                cardgame::player("Bob", points = 100))
bots <- list(cardgame::poker_bot(risk = 0.4, adaptability = 0.2, seed = 1),
             cardgame::poker_bot(risk = 0.7, adaptability = 0.4, seed = 2))
result <- cardgame::simulate_poker_game(players, bots, hands = 3, seed = 42)
stopifnot(all(rowSums(result$net_chips) == 0))
result$net_chips
