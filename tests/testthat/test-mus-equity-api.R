test_that("Mus lance estimates report applicability and preserve public boundaries", {
  g <- new_mus(seed=3)
  o <- game_observation(g)
  set.seed(18);before <- .Random.seed
  report <- estimate_hand(o,n=50,seed=1)
  expect_identical(.Random.seed,before)
  expect_equal(report$accepted,50)
  expect_equal(report$estimates$lance,c("grande","chica","pares","juego","punto"))
  expect_true(all(report$estimates$estimate>=0 & report$estimates$estimate<=1,na.rm=TRUE))
  expect_true(all(is.na(report$estimates$lower[report$estimates$samples==0])))
  # The API receives no game/deck reference: different hidden allocations are irrelevant.
  g$players[[2]]$hand <- rev(g$players[[2]]$hand);g$deck <- rev(g$deck)
  expect_identical(report,estimate_hand(game_observation(g),n=50,seed=1))
})

test_that("inconsistent declarations exhaust the budget rather than inventing odds", {
  o <- game_observation(new_mus(seed=1))
  o$phase <- "pares";o$eligible <- 99L
  report <- mus_lance_equity(o,n=5,seed=1)
  expect_equal(report$attempts,50)
  expect_equal(report$accepted,0)
  expect_true(report$exhausted)
  expect_true(all(is.na(report$estimates$estimate)))
})
