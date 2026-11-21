# Optional local cost comparison; timings depend on hardware and game state.
# Run source(system.file("benchmarks", "statistics.R", package="cardgame")).
measure <- function(label, expression) {
  elapsed <- system.time(result <- force(expression))[["elapsed"]]
  data.frame(case=label,elapsed_seconds=elapsed,
    work=if(!is.null(result$evaluations)) result$evaluations else
      if(!is.null(result$completed)) result$completed else max(result$estimates$samples))
}
hole <- list(cardgame::card("A","Spades"),cardgame::card("K","Spades"))
board <- list(cardgame::card("2","Hearts"),cardgame::card("7","Clubs"),
              cardgame::card("9","Diamonds"),cardgame::card("J","Hearts"),cardgame::card("Q","Clubs"))
for(seed in 1:100) {
  bj <- cardgame::blackjack_game(seed=seed)
  if(!bj$complete) break
}
stopifnot(!bj$complete)
results <- rbind(
  measure("Poker river exact",cardgame::poker_equity(hole,board,method="exact")),
  measure("Poker preflop 1000 samples",cardgame::poker_equity(hole,n=1000,seed=1)),
  measure("Mus 100 accepted deals",cardgame::estimate_hand(cardgame::game_observation(cardgame::mus_game(paste0("P",1:4),seed=1)),n=100,seed=1)),
  measure("Blackjack 20 samples/action",cardgame::estimate_actions(cardgame::game_observation(bj),n=20,seed=1)),
  measure("Scopa 5 samples/action",cardgame::estimate_actions(cardgame::game_observation(cardgame::scopa_game(seed=1)),n=5,seed=1)),
  measure("Blackjack 100 full rounds",cardgame::simulate_games(
    function(i) cardgame::blackjack_game(),list(cardgame::blackjack_bot()),n=100,seed=1)))
print(results,row.names=FALSE)
