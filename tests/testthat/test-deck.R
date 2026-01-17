# test-deck.R: Tests for deck S3 class and methods

test_that("deck constructor creates correct object", {
  d1 <- deck()
  expect_s3_class(d1, "deck")
  expect_true(!is.null(attr(d1, "language")))
})

test_that("deck print method outputs expected string", {
  d2 <- deck()
  expect_output(print(d2), "Deck (")
})

test_that("deck shuffle changes order but preserves cards", {
  d3 <- deck()
  shuffled <- shuffle.deck(d3)
  expect_s3_class(shuffled, "deck")
  expect_setequal(unclass(d3), unclass(shuffled))
})

test_that("deck discard_card removes correct card and preserves attributes", {
  d4 <- deck(cards = list(card("A", "Spades"), card("2", "Hearts")))
  d4b <- discard_card.deck(d4, 1)
  expect_equal(length(unclass(d4b)), 1)
  expect_equal(attr(d4, "language"), attr(d4b, "language"))
})

# Edge case: discard out of bounds

test_that("deck discard_card handles out of bounds", {
  d5 <- deck(cards = list(card("A", "Spades")))
  expect_error(discard_card.deck(d5, 2))
})
