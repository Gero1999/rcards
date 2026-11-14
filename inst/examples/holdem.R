players <- list(rcards::player("Alice", points = 100),
                rcards::player("Bob", points = 100))
bots <- list(rcards::poker_bot(risk = 0.4, adaptability = 0.2, seed = 1),
             rcards::poker_bot(risk = 0.7, adaptability = 0.4, seed = 2))
result <- rcards::simulate_poker_game(players, bots, hands = 3, seed = 42)
stopifnot(all(rowSums(result$net_chips) == 0))
result$net_chips
