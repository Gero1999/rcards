game <- rcards::mus_game(c("Alice", "Bob", "Carol", "Dan"),
                        rules = rcards::mus_rules(kings = 8, points_to_win = 40),
                        seed = 42)
game <- rcards::mus_action(game, "cut")
while (!game$hand_complete) game <- rcards::mus_action(game, "pass")
stopifnot(length(game$scores) == 2, all(game$scores >= 0))
game$scores
