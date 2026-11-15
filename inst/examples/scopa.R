for (n in 2:4) {
  game <- rcards::scopa_game(paste0("P",seq_len(n)),seed=42)
  result <- rcards::play(game,rep(list(rcards::scopa_bot()),n))
  stopifnot(result$game$hand_complete)
}
# For a human and three bots (seats 1/3 are partners):
# rcards::play(rcards::scopa_game(c("You","Bot 1","Partner","Bot 2")),
#   c(list(rcards::console_player(language="it")),rep(list(rcards::scopa_bot()),3)),
#   until="game")
