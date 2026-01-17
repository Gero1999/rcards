# test-card.R: Tests for card S3 class and constructor

test_that("card constructor creates correct object", {
  c1 <- card("A", "Spades")
  expect_s3_class(c1, "card")
  expect_equal(c1$value, "A")
  expect_equal(c1$suit, "Spades")
  expect_true(grepl("A", c1$display))
  expect_true(nchar(c1$symbol) == 1)
})

test_that("card display uses Unicode symbol", {
  c2 <- card("10", "Hearts")
  expect_true(grepl("10", c2$display))
  expect_true(grepl(c2$symbol, c2$display))
})

test_that("card custom display is respected", {
  c3 <- card("K", "Diamonds", display = "King of Diamonds")
  expect_equal(c3$display, "King of Diamonds")
})

# Edge case: invalid suit
# (Assumes suit_symbols is defined elsewhere)
test_that("card handles invalid suit gracefully", {
  expect_error(card("A", "InvalidSuit"), NA)
})
