test_that("Blackjack action estimates ignore real hidden cards and preserve the caller", {
  g <- new_bj("Ts 9h 8d 8c")
  o <- game_observation(g)
  set.seed(5);before <- .Random.seed
  result <- estimate_actions(o,n=12,seed=1)
  expect_identical(.Random.seed,before)
  expect_setequal(result$estimates$action,game_legal_actions(g)$actions)
  expect_true(all(result$estimates$samples==12))
  g$dealer[[2]] <- card("2","Clubs");g$shoe <- rev(g$shoe)
  expect_identical(result,estimate_actions(game_observation(g),n=12,seed=1))
  expect_error(estimate_actions(o,n=100,max_evaluations=2),"exceed")
  expect_error(estimate_actions(o,n=1,controllers=list(console_player())),"non-console")
})

test_that("Blackjack sampled models respect the dealer natural check", {
  g <- new_bj("9s Ah 7d 6c")
  o <- game_observation(g)
  for(i in 1:20) {
    model <- blackjack_public_model(o)
    expect_false(blackjack_total(model$dealer)$natural)
    expect_equal(length(model$shoe)+length(model$dealer)+sum(vapply(model$hands[[1]],function(h) length(h$cards),0L)),52)
  }
})
