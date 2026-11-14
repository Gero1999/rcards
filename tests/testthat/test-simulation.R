passive_policy <- function(o) list(action=if("check" %in% o$legal$actions) "check" else "call")

test_that("simulations complete separate hands and conserve chips", {
  players <- lapply(1:3,function(i) player(paste0("P",i),points=50))
  result <- simulate_poker_game(players,rep(list(passive_policy),3),hands=4,seed=12)
  expect_true(all(vapply(result$hands,`[[`,FALSE,"complete")))
  expect_equal(rowSums(result$net_chips),rep(0,4))
  expect_equal(vapply(result$hands,`[[`,0,"dealer"),c(1,2,3,1))
  expect_identical(result,simulate_poker_game(players,rep(list(passive_policy),3),hands=4,seed=12))
})

test_that("statistical bots persist decision state across hands", {
  players <- list(player("A",points=30),player("B",points=30))
  bots <- list(poker_bot(equity_samples=16,seed=1),poker_bot(equity_samples=16,seed=2))
  result <- simulate_poker_game(players,bots,hands=3,seed=15)
  expect_true(all(vapply(result$bots,`[[`,0,"decisions") > 0))
  expect_equal(rowSums(result$net_chips),rep(0,3))
  expect_identical(result,simulate_poker_game(players,bots,hands=3,seed=15))
  expect_error(simulate_poker_game(players,rep(list(passive_policy),2),max_actions=1,seed=2),"max_actions")
  expect_error(simulate_poker_game(players,list(function(o) list(action="invalid"),passive_policy)),"Illegal")
})
