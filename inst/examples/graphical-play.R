# Optional local browser play. Install shiny separately before running.
# These examples use the same engines as console play and simulations.

if (interactive()) {
  rcards::play_app()  # select the game, language and players; click New table

  # Or start a configured game, including a fixed human viewer seat.
  rcards::play_app(rcards::scopa_game(seed = 42), language = "it")
}
