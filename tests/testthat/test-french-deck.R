test_that("French decks have unique standard cards and explicit jokers", {
  standard <- deck()
  expect_length(standard, 52)
  keys <- vapply(standard, function(x) paste(x$value, x$suit), character(1))
  expect_length(unique(keys), 52)
  expect_true(all(vapply(standard, inherits, logical(1), "card")))
  expect_length(attr(standard, "optional_cards"), 0)
  expect_length(deck(include_optional = TRUE), 54)
  expect_length(deck(cards = list()), 0)
  expect_error(deck(include_optional = NA), "include_optional")
})

test_that("bundled French data matches the generator", {
  e <- new.env()
  data("deck_french", package = "rcards", envir = e)
  expect_identical(e$deck_french, deck(include_optional = TRUE))
})
