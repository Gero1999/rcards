parse_oracle_hand <- function(text) {
  suits <- c(s="Spades", h="Hearts", d="Diamonds", c="Clubs")
  lapply(strsplit(text," ")[[1]],function(code) {
    value <- substr(code,1,1)
    if(value=="T") value <- "10"
    card(value,unname(suits[substr(code,2,2)]))
  })
}

test_that("poker ordering agrees with independent Treys fixtures", {
  oracle <- utils::read.csv(testthat::test_path("fixtures","treys-rankings.csv"),stringsAsFactors=FALSE)
  expect_equal(nrow(oracle),180)
  for(i in seq_len(nrow(oracle))) {
    lhs <- poker_hand_points(parse_oracle_hand(oracle$left[i]))
    rhs <- poker_hand_points(parse_oracle_hand(oracle$right[i]))
    expect_equal(sign(lhs-rhs),oracle$expected[i],info=paste("Independent fixture",i))
  }
})

test_that("accepted Mus wagers wait for showdown and do not preempt a later ordago", {
  g <- mus_action(new_mus(seed=3),"cut")
  g$scores <- c(39,39)
  g <- mus_action(g,"bet",amount=2)
  g <- mus_action(g,"accept")
  expect_equal(g$phase,"chica")
  expect_equal(g$scores,c(39,39))
  expect_false(g$complete)
  expect_false(g$revealed)
  g <- mus_action(g,"ordago")
  expected <- mus_winner(g)
  g <- mus_action(g,"accept")
  expect_true(g$complete)
  expect_equal(g$winner,expected)
  expect_true(g$revealed)
  expect_equal(length(g$results),1)
  expect_equal(g$results[[1]]$lance,"chica")
})

test_that("Mus refusal points are immediate but intrinsic points wait", {
  g <- mus_action(new_mus(seed=3),"cut")
  g <- mus_action(g,"bet",amount=2)
  team <- g$bet_team
  g <- mus_action(g,"reject")
  expect_equal(g$scores[team],1)
  expect_false(g$revealed)
  finished <- finish_mus_passing(g)
  expect_true(finished$hand_complete)
  expect_true(finished$revealed)
  # Recorded settlements follow the lance order even after a refusal.
  lances <- vapply(finished$results,`[[`,"","lance")
  expect_true(all(diff(match(lances,c("grande","chica","pares","juego","punto")))>=0))
})

test_that("Mus hand-end settlement stops at the first game-winning lance", {
  g <- mus_action(new_mus(seed=3),"cut")
  g$scores <- c(39,39)
  finished <- finish_mus_passing(g)
  expect_true(finished$complete)
  expect_equal(length(finished$results),1)
  expect_equal(finished$results[[1]]$lance,"grande")
})
