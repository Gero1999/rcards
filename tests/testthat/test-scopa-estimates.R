test_that("Scopa action models reconstruct public captures and forty unique cards", {
  for(n in 2:4) {
    g <- scopa_game(paste0("P",1:n),seed=2)
    for(turn in 1:5) g <- game_action(g,choose_action(scopa_bot(),game_observation(g))$decision)
    o <- game_observation(g)
    model <- scopa_public_model(o)
    cards <- c(model$stock,model$table_cards,unlist(model$captured,recursive=FALSE),
      unlist(lapply(model$players,`[[`,"hand"),recursive=FALSE))
    expect_length(italian_card_ids(cards),40)
    expect_identical(model$captured,g$captured)
    expect_identical(model$last_capturer,g$last_capturer)
    expect_identical(model$players[[o$actor]]$hand,o$hole)
  }
})

test_that("Scopa rollouts are seeded conditional on public observations only", {
  g <- scopa_game(seed=1);o <- game_observation(g)
  set.seed(3);before <- .Random.seed
  result <- estimate_actions(o,n=3,seed=4)
  expect_identical(.Random.seed,before)
  expect_true(all(result$estimates$samples==3))
  expect_equal(result$type,"scopa_action_margin")
  g$stock <- rev(g$stock);g$players[[2]]$hand <- rev(g$players[[2]]$hand)
  expect_identical(result,estimate_actions(game_observation(g),n=3,seed=4))
  expect_error(estimate_actions(o,n=100,max_evaluations=1),"exceed")
  bad <- o;bad$capture_counts[1]<-1L
  expect_error(scopa_public_model(bad),"history")
})

test_that("unfinished action rollouts abort rather than fabricate point outcomes", {
  o <- game_observation(scopa_game(seed=1))
  set.seed(10);before <- .Random.seed
  expect_error(estimate_actions(o,n=1,max_actions=1,seed=2),"incomplete")
  expect_identical(.Random.seed,before)
})
