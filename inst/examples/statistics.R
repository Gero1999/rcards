# Card-only estimates: uniform unknown opponents, no betting model.
hole <- list(cardgame::card("A","Spades"),cardgame::card("K","Spades"))
equity <- cardgame::poker_equity(hole,n=100,seed=42)
stopifnot(equity$method=="monte_carlo")
cardgame::draw_probability(successes=9,remaining=47,draws=1)

# Strategy-dependent independent rounds. Controllers reset each run.
factory <- function(i) cardgame::blackjack_game(list(cardgame::player("Alice",points=100)))
report <- cardgame::simulate_games(factory,list(cardgame::blackjack_bot()),n=10,seed=42)
stopifnot(report$completed==10)

# Action estimates keep the public observation fixed, resampling hidden cards.
game <- cardgame::scopa_game(seed=42)
actions <- cardgame::estimate_actions(cardgame::game_observation(game),n=2,seed=42)
stopifnot(actions$type=="scopa_action_margin")
