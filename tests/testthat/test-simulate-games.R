test_that("batch simulations report profit correctly and preserve seeded caller state", {
  factory <- function(i) holdem_hand(list(player("A",points=20),player("B",points=20)))
  set.seed(6);before <- .Random.seed
  result <- simulate_games(factory,rep(list(passive_controller),2),n=8,seed=1)
  expect_identical(.Random.seed,before)
  expect_equal(result$completed,8)
  expect_equal(result$incomplete,0)
  expect_equal(sum(result$runs$value),0)
  expect_true(all(result$summary$samples==8))
  expect_identical(result,simulate_games(factory,rep(list(passive_controller),2),n=8,seed=1))
  # Auto-settled all-in constructors still use original per-seat funds.
  result <- simulate_games(function(i) holdem_hand(list(player("A",points=1),player("B",points=1))),
    rep(list(passive_controller),2),n=2,seed=3)
  expect_equal(sum(result$runs$value),0)
})

test_that("Blackjack and partnership games use explicit outcome definitions", {
  bj <- simulate_games(function(i) blackjack_game(list(player("A",points=100))),list(blackjack_bot()),n=5,seed=2)
  expect_equal(bj$completed,5)
  expect_match(bj$definition,"profit")
  scopa <- simulate_games(function(i) scopa_game(paste0("P",1:4),rules=scopa_rules(target=2)),
    rep(list(scopa_bot()),4),n=2,until="game",seed=1)
  expect_equal(scopa$completed,2)
  expect_equal(sum(scopa$summary$win_probability),1)
  mus <- simulate_games(function(i) mus_game(paste0("P",1:4)),rep(list(passive_mus_controller),4),n=2,seed=3)
  expect_equal(nrow(mus$summary),2)
  expect_match(mus$definition,"deal gains")
})

test_that("incomplete trials are explicit and never counted as losses", {
  factory <- function(i) holdem_hand(list(player("A",points=100),player("B",points=100)))
  expect_error(simulate_games(factory,rep(list(passive_controller),2),n=1,max_actions=1),"incomplete")
  report <- simulate_games(factory,rep(list(passive_controller),2),n=2,max_actions=1,on_incomplete="record")
  expect_equal(report$completed,0)
  expect_equal(report$incomplete,2)
  expect_equal(nrow(report$summary),0)
  expect_error(simulate_games(factory,rep(list(console_player()),2),n=1),"non-console")
})
