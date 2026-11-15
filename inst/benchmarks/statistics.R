# Optional local cost comparison; timings depend on hardware and game state.
# Run source(system.file("benchmarks", "statistics.R", package="rcards")).
measure <- function(label, expression) {
  elapsed <- system.time(result <- force(expression))[["elapsed"]]
  data.frame(case=label,elapsed_seconds=elapsed,
    work=if(!is.null(result$evaluations)) result$evaluations else
      if(!is.null(result$completed)) result$completed else max(result$estimates$samples))
}
hole <- list(rcards::card("A","Spades"),rcards::card("K","Spades"))
board <- list(rcards::card("2","Hearts"),rcards::card("7","Clubs"),
              rcards::card("9","Diamonds"),rcards::card("J","Hearts"),rcards::card("Q","Clubs"))
for(seed in 1:100) {
  bj <- rcards::blackjack_game(seed=seed)
  if(!bj$complete) break
}
stopifnot(!bj$complete)
results <- rbind(
  measure("Poker river exact",rcards::poker_equity(hole,board,method="exact")),
  measure("Poker preflop 1000 samples",rcards::poker_equity(hole,n=1000,seed=1)),
  measure("Mus 100 accepted deals",rcards::estimate_hand(rcards::game_observation(rcards::mus_game(paste0("P",1:4),seed=1)),n=100,seed=1)),
  measure("Blackjack 20 samples/action",rcards::estimate_actions(rcards::game_observation(bj),n=20,seed=1)),
  measure("Scopa 5 samples/action",rcards::estimate_actions(rcards::game_observation(rcards::scopa_game(seed=1)),n=5,seed=1)),
  measure("Blackjack 100 full rounds",rcards::simulate_games(
    function(i) rcards::blackjack_game(),list(rcards::blackjack_bot()),n=100,seed=1)))
print(results,row.names=FALSE)
