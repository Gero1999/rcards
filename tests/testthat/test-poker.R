# test-poker.R: Tests for poker logic and functions

test_that("poker_hand_points ranks hands correctly", {
  h1 <- list(card("A", "Spades"), card("K", "Spades"), card("Q", "Spades"), card("J", "Spades"), card("10", "Spades")) # Royal Flush
  h2 <- list(card("2", "Hearts"), card("3", "Hearts"), card("4", "Hearts"), card("5", "Hearts"), card("6", "Hearts")) # Straight Flush
  h3 <- list(card("A", "Hearts"), card("A", "Diamonds"), card("A", "Clubs"), card("A", "Spades"), card("2", "Hearts")) # Four of a Kind
  expect_true(poker_hand_points(h1) > poker_hand_points(h2))
  expect_true(poker_hand_points(h2) > poker_hand_points(h3))
})

test_that("evaluate_poker_hands returns correct winner(s)", {
  game <- list(players = list(
    list(hand = list(card("A", "Spades"), card("K", "Spades"), card("Q", "Spades"), card("J", "Spades"), card("10", "Spades"))),
    list(hand = list(card("2", "Hearts"), card("3", "Hearts"), card("4", "Hearts"), card("5", "Hearts"), card("6", "Hearts")))
  ))
  winners <- evaluate_poker_hands(game, players = 1:2)
  expect_equal(winners, 1)
})

test_that("determine_poker_winner splits pot for ties", {
  game <- list(
    players = list(
      list(hand = list(card("A", "Spades"), card("A", "Hearts"), card("A", "Diamonds"), card("A", "Clubs"), card("2", "Spades")), points = 0),
      list(hand = list(card("A", "Spades"), card("A", "Hearts"), card("A", "Diamonds"), card("A", "Clubs"), card("2", "Hearts")), points = 0)
    ),
    table = list(pots = list(list(amount = 100, eligible_players = 1:2))),
    deck = list()
  )
  game <- determine_poker_winner(game, players = 1:2, pot = 1)
  expect_equal(game$players[[1]]$points, 50)
  expect_equal(game$players[[2]]$points, 50)
  expect_equal(game$table$pots[[1]]$amount, 0)
})

test_that("player_bet_poker deducts and adds to pot", {
  game <- list(
    players = list(list(points = 100), list(points = 100)),
    table = list(pots = list(list(amount = 0))),
    deck = list()
  )
  game <- player_bet_poker(game, 1, 20, 1)
  expect_equal(game$players[[1]]$points, 80)
  expect_equal(game$table$pots[[1]]$amount, 20)
})

test_that("player_bet_poker errors if bet too high", {
  game <- list(players = list(list(points = 10)), table = list(pots = list(list(amount = 0))), deck = list())
  expect_error(player_bet_poker(game, 1, 20, 1))
})

test_that("player_discard_poker replaces cards and updates decks", {
  # Minimal mock for discard_card.deck and deck
  mock_deck <- list(card("A", "Spades"), card("2", "Hearts"), card("3", "Clubs"))
  attr(mock_deck, "class") <- "deck"
  attr(mock_deck, "main_cards") <- mock_deck
  attr(mock_deck, "optional_cards") <- NULL
  attr(mock_deck, "language") <- "en"
  game <- list(
    players = list(list(hand = list(card("A", "Spades"), card("2", "Hearts")))),
    deck = mock_deck,
    table = list(deck = mock_deck)
  )
  game2 <- player_discard_poker(game, 1, 1)
  expect_length(game2$players[[1]]$hand, 2)
})

test_that("player_raise_poker handles fold, call, and raise", {
  game <- list(
    players = list(list(points = 100), list(points = 100)),
    table = list(pots = list(list(amount = 0, eligible_players = 1:2, contributions = c(0,0)))),
    deck = list()
  )
  # Fold
  g2 <- player_raise_poker(game, 1, NULL, 1)
  expect_false(1 %in% g2$table$pots[[1]]$eligible_players)
  # Call
  g3 <- player_raise_poker(game, 2, 0, 1)
  expect_equal(g3$players[[2]]$points, 100)
  # Raise
  g4 <- player_raise_poker(game, 2, 10, 1)
  expect_equal(g4$table$pots[[1]]$contributions[2], 10)
})
