# test-player.R: Tests for player S3 class and methods

test_that("player constructor creates correct object", {
  p1 <- player(name = "Alice", points = 100)
  expect_s3_class(p1, "player")
  expect_equal(p1$name, "Alice")
  expect_equal(p1$points, 100)
  expect_equal(p1$role, "standard")
  expect_type(p1$hand, "list")
})

test_that("print.player outputs expected string", {
  p2 <- player()
  expect_output(print(p2), "Player (")
})

test_that("take_cards adds cards to hand and updates deck", {
  d <- deck(cards = list(card("A", "Spades"), card("2", "Hearts")))
  p <- player()
  res <- take_cards(p, d, n = 1)
  expect_s3_class(res$player, "player")
  expect_length(res$player$hand, 1)
  expect_s3_class(res$deck, "deck")
  expect_length(unclass(res$deck), 1)
})

test_that("leave_cards removes cards from hand and updates deck", {
  c1 <- card("A", "Spades")
  c2 <- card("2", "Hearts")
  p <- player(hand = list(c1, c2))
  d <- deck(cards = list())
  res <- leave_cards(p, d, 1)
  expect_length(res$player$hand, 1)
  expect_length(unclass(res$deck), 1)
})

# Edge case: leave_cards with invalid index

test_that("leave_cards handles invalid index", {
  c1 <- card("A", "Spades")
  p <- player(hand = list(c1))
  d <- deck(cards = list())
  expect_error(leave_cards(p, d, 2))
})
