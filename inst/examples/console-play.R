# Source this file in an interactive R session to play one hand against a bot.
# Batch scripts can use play() with policy functions or injected console callbacks.
if (interactive()) {
  players <- list(rcards::player("You", points = 100),
                  rcards::player("Bot", points = 100))
  played <- rcards::play(
    rcards::holdem_hand(players, seed = 42),
    list(rcards::console_player(), rcards::poker_bot(seed = 1))
  )
}
