for (n in 2:4) {
  game <- cardgame::scopa_game(paste0("P",seq_len(n)),seed=42)
  result <- cardgame::play(game,rep(list(cardgame::scopa_bot()),n))
  stopifnot(result$game$hand_complete)
}
# For a human and three bots (seats 1/3 are partners):
# cardgame::play(cardgame::scopa_game(c("You","Bot 1","Partner","Bot 2")),
#   c(list(cardgame::console_player(language="it")),rep(list(cardgame::scopa_bot()),3)),
#   until="game")
