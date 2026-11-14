new_holdem <- function(stacks = c(100,100), ...) {
  holdem_hand(lapply(seq_along(stacks), function(i) player(paste0("P", i), points = stacks[i])), ...)
}
finish_calling <- function(h) {
  limit <- 0L
  while (!h$complete) {
    legal <- holdem_legal_actions(h)
    h <- holdem_action(h, if ("check" %in% legal$actions) "check" else "call")
    limit <- limit + 1L
    if (limit > 200) stop("Hand failed to terminate")
  }
  h
}

test_that("heads-up blinds and acting order are correct", {
  h <- new_holdem(seed=5)
  expect_equal(h$actor, 1)
  expect_equal(h$contributions, c(1,2))
  h <- holdem_action(h, "call")
  expect_equal(h$actor, 2)
  h <- holdem_action(h, "check")
  expect_equal(h$street, "flop")
  expect_equal(h$actor, 2)
  expect_length(h$board, 3)
  done <- finish_calling(h)
  expect_equal(sum(holdem_stacks(done)), 200)
  expect_length(done$board, 5)
  expect_length(done$burned, 3)
  expect_length(unique(poker_card_ids(c(unlist(lapply(done$players, `[[`, "hand"), recursive=FALSE),
    done$board, done$burned, as_card_list(done$deck)))), 52)
})

test_that("folds and unequal all-ins conserve chips and form side pots", {
  folded <- holdem_action(new_holdem(seed=7), "fold")
  expect_true(folded$complete)
  expect_equal(holdem_stacks(folded), c(99,101))
  h <- new_holdem(c(10,30,60), seed=7)
  h <- holdem_action(h, "raise", 10)
  h <- holdem_action(h, "raise", 30)
  h <- holdem_action(h, "call")
  expect_true(h$complete)
  expect_equal(vapply(h$pots, `[[`, 0, "amount"), c(30,40))
  expect_equal(h$pots[[2]]$eligible_players, c(2,3))
  expect_equal(sum(holdem_stacks(h)), 100)
  short <- finish_calling(new_holdem(c(1,100),seed=8))
  expect_equal(sum(holdem_stacks(short)),101)
})

test_that("short all-ins do not reopen a previous full raiser", {
  h <- new_holdem(c(100,100,12),seed=8)
  h <- holdem_action(h,"raise",10)
  h <- holdem_action(h,"call")
  h <- holdem_action(h,"raise",12)
  expect_false("raise" %in% holdem_legal_actions(h)$actions)
  expect_error(holdem_action(h,"raise",20),"Illegal")
  expect_equal(sum(holdem_stacks(finish_calling(h))),212)
})

test_that("invalid actions and stacks are rejected, and seeds preserve RNG", {
  set.seed(42); before <- .Random.seed
  h <- new_holdem(seed=99)
  expect_identical(.Random.seed,before)
  expect_identical(new_holdem(seed=99),h)
  expect_error(holdem_action(h,"check"),"Illegal")
  expect_error(holdem_action(h,"raise",3),"raise-to")
  expect_error(new_holdem(c(-1,20)),"stack")
  expect_error(holdem_hand(list(player(points=10))),"two to ten")
})

test_that("a tied board splits integer chips without losing odd chips", {
  h <- new_holdem(c(10,10,10),seed=1)
  h$board <- Map(card,c("10","J","Q","K","A"),rep("Spades",5))
  h$contributions <- c(5,5,5);h$folded <- c(FALSE,FALSE,TRUE)
  for(i in 1:3) { h$players[[i]]$points <- 5; h$players[[i]]$hand <-
    list(card(as.character(i+1),"Hearts"),card(as.character(i+1),"Clubs")) }
  h <- holdem_finish(h)
  expect_equal(holdem_stacks(h),c(12,13,5))
})
