test_that("observations do not disclose opponent cards or the deck", {
  h <- new_holdem(seed=11)
  o <- holdem_observation(h)
  expect_false(any(c("players","deck","burned") %in% names(o)))
  expect_identical(o$hole,h$players[[h$actor]]$hand)
  altered <- h
  altered$players[[3L-h$actor]]$hand <- list(card("Joker","Joker"))
  expect_identical(holdem_observation(altered),o)
})

test_that("bots return reproducible legal decisions and retain state", {
  h <- new_holdem(seed=21)
  bot <- poker_bot(equity_samples=32,seed=7)
  set.seed(100); before <- .Random.seed
  a <- poker_bot_action(bot,holdem_observation(h))
  expect_identical(.Random.seed,before)
  expect_identical(a,poker_bot_action(bot,holdem_observation(h)))
  expect_true(a$action %in% holdem_legal_actions(h)$actions)
  expect_equal(a$bot$decisions,1)
  expect_s3_class(holdem_action(h,a$action,a$amount),"holdem_hand")
})

test_that("risk adaptation and variation are independent controls", {
  h <- new_holdem(seed=21)
  o <- holdem_observation(h)
  o$history <- list(list(player=3L-o$actor,action="raise"))
  fixed <- poker_bot_action(poker_bot(risk=.5,adaptability=0,variation=0,equity_samples=8,seed=2),o)
  adaptive <- poker_bot_action(poker_bot(risk=.5,adaptability=1,variation=0,equity_samples=8,seed=2),o)
  expect_equal(fixed$effective_risk,.5)
  expect_lt(adaptive$effective_risk,.5)
  expect_equal(adaptive$effective_risk,adaptive$bot$current_risk)
  expect_error(poker_bot(risk=2),"risk")
  expect_error(poker_bot(equity_samples=0),"equity_samples")
})
