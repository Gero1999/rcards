test_that("heads-up continuation retains settled stacks and rotates blinds", {
  h <- new_holdem(seed=1)
  h$players[[1]]$role <- "human"
  done <- holdem_action(h,"fold")
  before <- done
  next_hand <- holdem_next_hand(done,seed=2)
  expect_equal(next_hand$initial_stacks,c(99,101))
  expect_equal(holdem_stacks(next_hand),c(97,100))
  expect_equal(next_hand$contributions,c(2,1))
  expect_equal(next_hand$dealer,2)
  expect_equal(next_hand$actor,2)
  expect_equal(next_hand$hand_number,2)
  expect_equal(next_hand$players[[1]]$role,"human")
  expect_identical(done,before)
  expect_length(next_hand$board,0)
  expect_length(next_hand$history,0)
  expect_equal(sum(holdem_stacks(next_hand))+sum(next_hand$contributions),200)
  expect_error(game_next_hand(done),NA)
})

test_that("repeated folded hands use one bankroll without refilling stacks", {
  h <- new_holdem(seed=1)
  for(i in 1:10) {
    h <- holdem_action(h,"fold")
    expect_equal(sum(holdem_stacks(h)),200)
    h <- holdem_next_hand(h,seed=i+1L)
    expect_equal(h$dealer,if(i %% 2L) 2L else 1L)
    expect_equal(h$hand_number,i+1L)
  }
  # Ten alternating small-blind folds cancel out; only this hand's blinds
  # have left the stacks. Resetting to 100 each time would fail prior checks.
  expect_equal(h$initial_stacks,c(100,100))
})

test_that("funded heads-up seats retain indices and skip eliminated seats", {
  h <- finish_calling(new_holdem(c(10,10,10,10),dealer=1,seed=1))
  # An explicit settled table with two funded, nonadjacent seats.
  for(i in 1:4) h$players[[i]]$points <- c(20,0,20,0)[i]
  next_hand <- holdem_next_hand(h,seed=2)
  expect_length(next_hand$players,4)
  expect_equal(next_hand$dealer,3)
  expect_equal(next_hand$contributions,c(2,0,1,0))
  expect_equal(next_hand$actor,3)
  expect_equal(vapply(next_hand$players,function(p) length(p$hand),0L),c(2,0,2,0))
  expect_equal(next_hand$folded,c(FALSE,TRUE,FALSE,TRUE))
  expect_false(any(c(2,4) %in% next_hand$pending))
  expect_length(next_hand$deck,48)
  next_hand <- holdem_action(next_hand,"fold")
  next_hand <- holdem_next_hand(next_hand,seed=3)
  expect_equal(next_hand$dealer,1)
  expect_equal(next_hand$contributions,c(1,0,2,0))
})

test_that("three-player all-ins transition to two without losing chips or seats", {
  h <- new_holdem(c(10,30,30),seed=1)
  h$board <- Map(card,c("A","K","Q","2","3"),c("Spades","Spades","Spades","Hearts","Clubs"))
  h$players[[1]]$hand <- list(card("J","Spades"),card("10","Spades"))
  h$players[[2]]$hand <- list(card("A","Hearts"),card("A","Clubs"))
  h$players[[3]]$hand <- list(card("K","Hearts"),card("K","Clubs"))
  h$contributions <- c(10,30,30)
  for(i in 1:3) h$players[[i]]$points <- 0
  done <- holdem_finish(h)
  expect_equal(holdem_stacks(done),c(30,40,0))
  next_hand <- holdem_next_hand(done,seed=2)
  expect_equal(next_hand$dealer,2)
  expect_equal(next_hand$contributions,c(2,1,0))
  expect_equal(next_hand$actor,2)
  expect_length(next_hand$players[[3]]$hand,0)
  expect_equal(sum(holdem_stacks(next_hand))+sum(next_hand$contributions),70)
  expect_equal(sum(holdem_stacks(finish_calling(next_hand))),70)
})

test_that("continuation shuffles reproducibly without retaining private state", {
  done <- holdem_action(new_holdem(seed=1),"fold")
  set.seed(314);before <- .Random.seed
  a <- holdem_next_hand(done,seed=2)
  expect_identical(.Random.seed,before)
  expect_identical(a,holdem_next_hand(done,seed=2))
  expect_false(identical(a$players[[1]]$hand,done$players[[1]]$hand))
  all_cards <- c(as_card_list(a$deck),unlist(lapply(a$players,`[[`,"hand"),recursive=FALSE))
  expect_length(unique(poker_card_ids(all_cards)),52)
  expect_error(holdem_next_hand(new_holdem(seed=1)),"Finish")
  terminal <- done
  terminal$players[[1]]$points <- 0
  terminal$players[[2]]$points <- 200
  expect_error(holdem_next_hand(terminal),"two funded")
  expect_error(new_holdem(c(0,100)),"stack")
})
