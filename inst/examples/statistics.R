# Card-only estimates: uniform unknown opponents, no betting model.
hole <- list(rcards::card("A","Spades"),rcards::card("K","Spades"))
equity <- rcards::poker_equity(hole,n=100,seed=42)
stopifnot(equity$method=="monte_carlo")
rcards::draw_probability(successes=9,remaining=47,draws=1)

# Strategy-dependent independent rounds. Controllers reset each run.
factory <- function(i) rcards::blackjack_game(list(rcards::player("Alice",points=100)))
report <- rcards::simulate_games(factory,list(rcards::blackjack_bot()),n=10,seed=42)
stopifnot(report$completed==10)

# Action estimates keep the public observation fixed, resampling hidden cards.
game <- rcards::scopa_game(seed=42)
actions <- rcards::estimate_actions(rcards::game_observation(game),n=2,seed=42)
stopifnot(actions$type=="scopa_action_margin")
