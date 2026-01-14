
# rcards: Play, Create, and Analyze Card Games in R

`rcards` is an R package for simulating, analyzing, and playing card games. It provides tools to create and shuffle decks, deal hands, define custom games, and run simulations for classic games like Poker, Blackjack, and more. The package is designed for both game enthusiasts and researchers interested in card game mechanics and probability.

## Features

- Create and customize card decks (standard, French, Spanish, or your own)
- Shuffle, deal, and manipulate decks and hands
- Simulate classic card games (Poker, Blackjack, etc.)
- Analyze hand probabilities and outcomes
- Build and test your own card game logic

## Installation

`rcards` is not yet on CRAN. To install the development version:

```r
# Clone the repository
git clone https://github.com/Gero1999/rcards

# In R:
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::load_all("/path/to/rcards")
```

Replace `/path/to/rcards` with the path where you cloned the repo.

## Quick Start

```r
# Load the package (after devtools::load_all or installation)
library(rcards)

# Create a standard deck and shuffle
deck <- create_deck(type = "french")
deck <- shuffle_deck(deck)

# Deal hands to players
hands <- deal_hands(deck, n_players = 4, hand_size = 5)

# Simulate a poker game (see ?simulate_poker_game for details)
# game <- setup_poker_game(players = c("Alice", "Bob"))
# result <- simulate_poker_game(game, deal_hand_fn, get_action_fn)
```

## Documentation

See the package documentation for details on all functions:

- Deck creation and manipulation: `create_deck`, `shuffle_deck`, `deal_hands`, etc.
- Game simulation: `simulate_poker_game`, `player_raise_poker`, etc.
- Data sets: Pre-built decks in `data/`

## Contributing

Contributions are welcome! If you have ideas for new games, features, or improvements, please open an issue or submit a pull request. See the [contributing guidelines - yet not available]() for more information.

