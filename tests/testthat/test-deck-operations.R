test_that("drawing and returning conserves cards and optional metadata", {
  d <- deck(include_optional = TRUE, language = "es")
  original <- d
  drawn <- take_cards(player(), d, 54)
  expect_length(drawn$deck, 0)
  expect_length(drawn$player$hand, 54)
  expect_length(attr(drawn$player$hand, "optional_cards"), 2)
  returned <- leave_cards(drawn$player, drawn$deck, seq_len(54))
  expect_identical(returned$deck, original)
  expect_length(returned$player$hand, 0)
  expect_identical(d, original)
})

test_that("subsetting and shuffling preserve coherent deck attributes", {
  d <- deck(include_optional = TRUE)
  jokers <- d[53:54]
  expect_s3_class(jokers, "deck")
  expect_length(attr(jokers, "main_cards"), 0)
  expect_length(attr(jokers, "optional_cards"), 2)
  set.seed(47)
  first <- shuffle(d)
  set.seed(47)
  expect_identical(first, shuffle(d))
  expect_setequal(vapply(first, `[[`, "", "display"), vapply(d, `[[`, "", "display"))
  expect_length(shuffle(deck(cards = list())), 0)
  expect_identical(discard_card(d, integer()), d)
})

test_that("draw and return reject invalid input and allow zero cards", {
  p <- player()
  d <- deck()
  expect_identical(take_cards(p, d, 0)$deck, d)
  expect_identical(leave_cards(p, d, integer())$deck, d)
  for (n in list(-1, 1.5, NA_real_, Inf, 53, c(1, 2))) {
    expect_error(take_cards(p, d, n), "Invalid")
  }
  for (i in list(0, -1, 1.5, NA_real_, Inf, 53, c(1, 1))) {
    expect_error(discard_card(d, i), "Index")
  }
  expect_error(d[53], "Index")
  expect_error(deck(cards = list("invalid")), "card objects")
})

test_that("returning an optional card preserves its classification", {
  d <- deck(cards = list(card("A", "Spades")),
            optional_cards = list(card("8", "Oros")), include_optional = TRUE)
  result <- take_cards(player(), d, 2)
  returned <- leave_cards(result$player, result$deck, 2)
  expect_length(attr(returned$deck, "optional_cards"), 1)
  expect_length(attr(returned$deck, "main_cards"), 0)
})

test_that("rcards tables do not replace base table printing", {
  expect_s3_class(rcards::table(), "rcards_table")
  expect_output(print(base::table(c("a", "a"))), "2")
})
