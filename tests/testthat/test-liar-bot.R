test_that("Mentiroso bots choose legal actions and complete seeded games", {
  for(style in c("spanish","french")) for(n in c(2,3,6,8)) {
    g <- liar_game(paste0("P",1:n),rules=liar_rules(style),seed=42)
    policies <- lapply(seq_len(n),function(i) liar_bot(seed=i))
    result <- play(g,policies,max_actions=10000L)
    expect_equal(result$status,"complete")
    expect_equal(length(result$game$players[[result$game$winner]]$hand),0)
    expect_error(liar_assert_cards(result$game),NA)
    expect_length(game_observation(result$game)$hole,0)
  }
})

test_that("bots challenge impossible claims and use only the public view", {
  g <- liar_game(seed=42)
  o <- game_observation(g)
  bot <- liar_bot(seed=1)
  set.seed(1);before <- .Random.seed
  a <- choose_action(bot,o)
  expect_identical(.Random.seed,before)
  g$players[[2]]$hand <- rev(g$players[[2]]$hand)
  expect_identical(a,choose_action(bot,game_observation(g)))
  o$phase <- "response";o$legal$actions <- c("challenge","pass")
  o$claim <- list(player=2L,rank=o$hole[[1]]$value,count=4L)
  expect_equal(choose_action(liar_bot(challenge_rate=0),o)$decision$action,"challenge")
})

test_that("Mentiroso simulations use confirmed wins and explicit remaining-card metrics", {
  result <- simulate_games(function(i) liar_game(seed=NULL),rep(list(liar_bot()),3),n=3,seed=2)
  expect_equal(result$completed,3)
  expect_equal(sum(result$summary$win_probability),1)
  expect_true(all(result$runs$value<=0))
  expect_match(result$definition,"confirmed empty-hand winner")
  result <- simulate_games(function(i) liar_game(),rep(list(liar_bot()),3),n=2,seed=2,
    max_actions=1,on_incomplete="record")
  expect_equal(result$completed,0)
  expect_equal(result$incomplete,2)
})

test_that("large numbered Liar hands still fit narrow console rows", {
  rows <- console_card_rows(as_card_list(mus_pack())[1:14],style="ascii",width=16,numbered=TRUE)
  expect_true(all(nchar(rows,type="width")<=16))
  expect_match(paste(rows,collapse="\n"),"[14]",fixed=TRUE)
})
