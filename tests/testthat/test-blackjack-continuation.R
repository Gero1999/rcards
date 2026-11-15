test_that("Blackjack next rounds retain bankrolls and base wagers after doubling", {
  g <- blackjack_step(new_bj("Ts 9h 6d 8c 5c"),"double")
  expect_equal(g$players[[1]]$points,120)
  expect_equal(g$wagers,10)
  before <- g
  g$players[[1]]$role <- "human"
  g$players[[1]]$note <- "retained metadata"
  next_round <- blackjack_next_round(g,pack=blackjack_pack("Ts 9h 8d 8c"))
  expect_equal(next_round$round_number,2)
  expect_equal(next_round$initial_stacks,120)
  expect_equal(next_round$players[[1]]$points,110)
  expect_equal(next_round$hands[[1]][[1]]$bet,10)
  expect_equal(next_round$players[[1]]$role,"human")
  expect_equal(next_round$players[[1]]$note,"retained metadata")
  expect_length(next_round$results,0)
  expect_length(next_round$history,0)
  expect_false(next_round$complete)
  expect_equal(before$players[[1]]$points,120)
  expect_equal(g$results[[1]]$bet,20)
  expect_length(next_round$shoe,48)
  expect_null(game_observation(next_round)$dealer[[2]])
})

test_that("split settlements and fractional naturals never change next base stakes", {
  split <- blackjack_step(new_bj("As 9h Ad 8c Ks Qh"),"split")
  expect_equal(split$players[[1]]$points,120)
  next_round <- blackjack_next_round(split,pack=blackjack_pack("Ts 9h 8d 8c"))
  expect_length(next_round$hands[[1]],1)
  expect_equal(next_round$hands[[1]][[1]]$bet,10)
  expect_equal(next_round$players[[1]]$points,110)

  natural <- new_bj("As 9h Kd 8c",wager=5)
  expect_equal(natural$players[[1]]$points,107.5)
  next_round <- blackjack_next_round(natural,pack=blackjack_pack("Ts 9h 8d 8c"))
  expect_equal(next_round$initial_stacks,107.5)
  expect_equal(next_round$players[[1]]$points,102.5)
  expect_equal(next_round$wagers,5)
})

test_that("unfunded Blackjack seats sit out without changing seat identities", {
  players <- lapply(seq_len(3),function(i) player(paste0("P",i),points=100))
  g <- blackjack_game(players,rules=blackjack_rules(decks=1),seed=3)
  while(!g$complete) g <- blackjack_step(g,"stand")
  g$players[[1]]$points <- 5
  g$players[[2]]$points <- 0
  g$players[[3]]$points <- 25
  next_round <- blackjack_next_round(g,pack=blackjack_pack("Ts 9h 8d 8c"))
  expect_equal(next_round$actor,3)
  expect_equal(vapply(next_round$players,`[[`,"","name"),c("P1","P2","P3"))
  expect_equal(vapply(next_round$players,`[[`,0,"points"),c(5,0,15))
  expect_length(next_round$hands[[1]],0)
  expect_length(next_round$hands[[2]],0)
  expect_length(next_round$hands[[3]],1)
  expect_equal(next_round$initial_stacks,c(5,0,25))
  expect_length(game_observation(next_round,1)$hands,0)
  next_round <- blackjack_step(next_round,"stand")
  expect_equal(next_round$results[[1]]$player,3)
  expect_equal(next_round$history[[1]]$player,3)
  expect_equal(vapply(next_round$players,`[[`,0,"points"),c(5,0,35))
  rejoined <- blackjack_next_round(next_round,wager=c(5,10,10),
    pack=blackjack_pack("Ts 8s 9h 7d 8d 8c"))
  expect_equal(rejoined$actor,1)
  expect_equal(vapply(rejoined$hands,length,0L),c(1L,0L,1L))
  expect_equal(rejoined$wagers,c(5,10,10))
  expect_equal(rejoined$round_number,3)
})

test_that("immediate Blackjack natural settlements use original seat indices", {
  g <- new_bj("As 9h Kd 8c")
  # A completed multi-seat table with a sitting-out first seat.
  first <- player("Out",points=0)
  g$players <- c(list(first),g$players)
  g$hands <- c(list(list()),g$hands)
  g$wagers <- c(10,10)
  next_round <- blackjack_next_round(g,pack=blackjack_pack("As 9h Kd 8c"))
  expect_true(next_round$complete)
  expect_null(next_round$actor)
  expect_equal(next_round$results[[1]]$player,2)
  expect_equal(vapply(next_round$players,`[[`,0,"points"),c(0,130))
  expect_length(next_round$hands[[1]],0)
  expect_equal(next_round$initial_stacks,c(0,115))
})

test_that("Blackjack continuation validates wagers and settled round boundaries", {
  active <- new_bj("Ts 9h 8d 8c")
  expect_error(blackjack_next_round(active),"Finish")
  g <- blackjack_step(active,"stand")
  before <- g
  for(wager in list(0,-1,NA_real_,Inf,"10",c(10,20)))
    expect_error(blackjack_next_round(g,wager=wager),"positive wagers")
  expect_identical(g,before)
  g$players[[1]]$points <- 5
  expect_error(blackjack_next_round(g),"No player can cover")
  low_wager <- blackjack_next_round(g,wager=5,pack=blackjack_pack("Ts 9h 8d 8c"))
  expect_equal(low_wager$players[[1]]$points,0)
  expect_equal(low_wager$hands[[1]][[1]]$bet,5)
  expect_error(blackjack_next_round(list()),"blackjack_game")
  expect_error(blackjack_next_round(before,pack=list()),"Pack")
})

test_that("Blackjack next-round seeds preserve caller RNG and use fresh shoes", {
  g <- blackjack_step(new_bj("Ts 9h 8d 8c"),"stand")
  set.seed(42);before <- .Random.seed
  a <- blackjack_next_round(g,seed=1)
  expect_identical(.Random.seed,before)
  expect_identical(a,blackjack_next_round(g,seed=1))
  expect_false(identical(a$shoe,blackjack_next_round(g,seed=2)$shoe))
  expect_equal(length(a$shoe)+length(a$dealer)+
    sum(vapply(a$hands[[1]],function(h) length(h$cards),0L)),52)
  expect_equal(a$round_number,2)
  expect_s3_class(game_next_hand(g),"blackjack_game")
})
