# Source this file in an interactive R session to play one hand against a bot.
# Batch scripts can use play() with policy functions or injected console callbacks.
if (interactive()) {
  players <- list(cardgame::player("You", points = 100),
                  cardgame::player("Bot", points = 100))
  played <- cardgame::play(
    cardgame::holdem_hand(players, seed = 42),
    list(cardgame::console_player(), cardgame::poker_bot(seed = 1))
  )
}
