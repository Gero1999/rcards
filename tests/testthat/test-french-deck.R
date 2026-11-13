test_that("deck construction is generic and empty by default", {
  expect_length(deck(), 0)
  expect_length(deck(include_optional = TRUE), 0)
  expect_length(deck(cards = list()), 0)
  expect_error(deck(include_optional = NA), "include_optional")
})

test_that("French data contains 52 standard cards and two optional jokers", {
  e <- new.env()
  data("deck_french", package = "rcards", envir = e)
  french <- e$deck_french
  standard <- deck(cards = attr(french, "main_cards"))
  expect_length(standard, 52)
  keys <- vapply(standard, function(x) paste(x$value, x$suit), character(1))
  expect_length(unique(keys), 52)
  expect_true(all(vapply(standard, inherits, logical(1), "card")))
  expect_length(attr(standard, "optional_cards"), 0)
  expect_length(attr(french, "optional_cards"), 2)
  expect_identical(french, deck(cards = attr(french, "main_cards"),
    optional_cards = attr(french, "optional_cards"), include_optional = TRUE))
})
