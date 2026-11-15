test_that("Mus bots receive only public observations and choose legal reproducible actions", {
  g <- new_mus(seed=3);o <- game_observation(g)
  bot <- mus_bot(equity_samples=8,seed=2)
  set.seed(42);before <- .Random.seed
  a <- choose_action(bot,o)
  expect_identical(.Random.seed,before)
  expect_identical(a,choose_action(bot,o))
  expect_true(a$decision$action %in% o$legal$actions)
  changed <- g;changed$players[[2]]$hand <- rev(changed$players[[2]]$hand)
  expect_identical(a,choose_action(bot,game_observation(changed)))
  expect_equal(a$controller$decisions,1)
  expect_error(mus_bot(risk=2),"risk")
  expect_error(choose_action(bot,game_observation(new_holdem(seed=1))),"Mus only")
})

test_that("Mus bots exchange and wager through the shared engine", {
  g <- new_mus(seed=3)
  for(i in 1:4) g <- mus_action(g,"mus")
  bot <- mus_bot(equity_samples=4,seed=2)
  choice <- choose_action(bot,game_observation(g))
  expect_equal(choice$decision$action,"discard")
  expect_s3_class(game_action(g,choice$decision),"mus_game")
  for(kings in c(4,8)) {
    g <- new_mus(rules=mus_rules(kings,30),seed=4)
    controllers <- lapply(1:4,function(i) mus_bot(equity_samples=4,seed=i))
    result <- play(g,controllers,until="game",seed=42,max_actions=2000)
    expect_equal(result$status,"complete")
    expect_true(max(result$game$scores)>=30)
    expect_true(all(vapply(result$controllers,`[[`,0,"decisions")>0))
    cards <- c(unlist(lapply(result$game$players,`[[`,"hand"),recursive=FALSE),as_card_list(result$game$deck),result$game$discard)
    expect_length(unique(mus_card_ids(cards)),40)
  }
})

test_that("a human console seat can play three Mus bots", {
  frame <- ""
  human <- console_player(read=function(prompt) {
    if(grepl("Actions: .*cut",frame)) return("cut")
    if(grepl("Actions: .*discard",frame)) return("discard none")
    if(grepl("Actions: .*accept",frame)) return("accept")
    "pass"
  },write=function(text) frame <<- text,style="ascii")
  controllers <- c(list(human),lapply(1:3,function(i) mus_bot(equity_samples=4,seed=i)))
  expect_output(result <- play(new_mus(seed=9),controllers,seed=5),NA)
  expect_true(result$status %in% c("hand_complete","complete"))
  expect_match(frame,"Scores")
  expect_true(all(vapply(result$controllers[2:4],`[[`,0,"decisions")>0))
})
