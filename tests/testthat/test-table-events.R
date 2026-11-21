test_that("Hold'em pot events announce a settled hand without uncalled returns", {
  hand <- new_holdem(seed = 7)
  expect_null(table_pot_event(hand, 1L))
  expect_null(table_pot_event(blackjack_game(seed = 7), 1L))

  settled <- holdem_action(hand, "fold")
  expect_equal(settled$payouts, c(0, 3))
  winner <- table_pot_event(settled, 2L)
  loser <- table_pot_event(settled, 1L)
  expect_identical(winner$key, "holdem-pot-1")
  expect_identical(loser$key, winner$key)
  expect_identical(winner$title, "You win the pot")
  expect_identical(loser$title, "%s wins the pot")
  expect_identical(loser$title_args, list("P2"))
  expect_identical(winner$tone, "win")
  expect_identical(loser$tone, "neutral")
  expect_identical(winner$detail[[1L]]$args, list("P2", "2"))
  expect_identical(loser$detail, winner$detail)
})

test_that("Hold'em events use actual shares when a split pot contains an odd chip", {
  hand <- new_holdem(c(10, 10, 10), seed = 1)
  hand$board <- lapply(c("10", "J", "Q", "K", "A"), card, suit = "Spades")
  hand$contributions <- c(5, 5, 5)
  hand$folded <- c(FALSE, FALSE, TRUE)
  for (i in seq_along(hand$players)) {
    hand$players[[i]]$points <- 5
    rank <- as.character(i + 1L)
    hand$players[[i]]$hand <- list(card(rank, "Hearts"), card(rank, "Clubs"))
  }
  settled <- holdem_finish(hand)
  event <- table_pot_event(settled, 1L)
  expect_identical(event$title, "Split pot")
  expect_identical(event$tone, "win")
  expect_identical(lapply(event$detail, `[[`, "args"), list(list("P1", "7"), list("P2", "8")))
  expect_identical(table_pot_event(settled, 3L)$tone, "neutral")
})

test_that("Hold'em events combine side-pot awards without calling them a split", {
  hand <- new_holdem(c(10, 30, 30), seed = 1)
  hand$board <- Map(card, c("A", "K", "Q", "2", "3"), c("Spades", "Spades", "Spades", "Hearts", "Clubs"))
  hand$players[[1L]]$hand <- list(card("J", "Spades"), card("10", "Spades"))
  hand$players[[2L]]$hand <- list(card("A", "Hearts"), card("A", "Clubs"))
  hand$players[[3L]]$hand <- list(card("K", "Hearts"), card("K", "Clubs"))
  for (i in seq_along(hand$players)) hand$players[[i]]$points <- 0
  hand$contributions <- c(10, 30, 30)
  settled <- holdem_finish(hand)

  event <- table_pot_event(settled, 2L)
  expect_identical(event$title, "Pots awarded")
  expect_identical(event$tone, "win")
  expect_identical(lapply(event$detail, `[[`, "args"), list(list("P1", "30"), list("P2", "40")))
})

test_that("table event markup escapes player names and identifies the announcement", {
  skip_if_not_installed("shiny")
  event <- list(key = "hand-x", title = "%s wins the pot", title_args = list("<a>"),
    detail = list(list(key = "%s: %s chips", args = list("<a>", "50"))), tone = "win")
  markup <- as.character(table_event_tag(event))
  expect_match(markup, 'data-rc-table-event="hand-x"', fixed = TRUE)
  expect_match(markup, 'class="rc-event rc-event-win"', fixed = TRUE)
  expect_match(markup, "&lt;a&gt; wins the pot", fixed = TRUE)
  expect_match(markup, "&lt;a&gt;: 50 chips", fixed = TRUE)
  expect_false(grepl("<a>", markup, fixed = TRUE))
  expect_null(table_event_tag(NULL))
})
