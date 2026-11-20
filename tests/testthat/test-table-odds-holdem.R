test_that("Hold'em odds snapshot uses the seat's cards and counts live opponents", {
  players <- lapply(c("A", "B", "C"), player, points = 100)
  game <- holdem_action(holdem_hand(players, seed = 12), "fold")
  observation <- game_observation(game, game$actor)
  snapshot <- table_holdem_odds_snapshot(observation)

  expect_identical(snapshot$opponents, 1L)
  expect_identical(snapshot$slots, c(poker_card_ids(observation$hole), rep("", 5L)))
  expect_null(table_holdem_odds_snapshot(game_observation(game, which(game$folded))))
})

test_that("Hold'em odds editors reject invalid cards and incomplete streets", {
  deck <- as_card_list(poker_pack())
  names(deck) <- poker_card_ids(deck)
  hole <- c("A:Spades", "J:Spades")
  flop <- c("K:Spades", "7:Spades", "2:Diamonds")

  expect_length(table_holdem_odds_scenario(c(hole, rep("", 5L)), deck, "en")$board, 0L)
  expect_length(table_holdem_odds_scenario(c(hole, flop, "", ""), deck, "en")$board, 3L)
  expect_error(table_holdem_odds_scenario(c(hole, flop, "", "3:Clubs"), deck, "en"), "turn card")
  expect_error(table_holdem_odds_scenario(c(hole, flop[1L], rep("", 4L)), deck, "en"), "three flop")
  expect_error(table_holdem_odds_scenario(c(hole, flop, "A:Spades", ""), deck, "en"), "only once")
  expect_error(table_holdem_odds_scenario(c(hole, flop, "A:Oros", ""), deck, "en"), "French deck")
})

test_that("Hold'em river flush chances are exact and distinct from showdown ties", {
  hole <- list(card("A", "Spades"), card("J", "Spades"))
  turn <- list(card("K", "Spades"), card("7", "Spades"), card("2", "Diamonds"), card("Q", "Hearts"))
  expect_equal(table_holdem_flush_chance(hole, turn), 9 / 46)
  expect_equal(table_holdem_flush_chance(hole, c(turn, list(card("4", "Spades")))), 1)
  expect_equal(table_holdem_flush_chance(hole, c(turn, list(card("4", "Clubs")))), 0)

  hole <- list(card("2", "Diamonds"), card("3", "Clubs"))
  board <- lapply(c("10", "J", "Q", "K", "A"), card, suit = "Hearts")
  report <- poker_equity(hole, board, opponents = 1L, method = "exact")
  expect_equal(table_holdem_flush_chance(hole, board), 1)
  expect_equal(report$estimates$estimate[report$estimates$metric == "win"], 0)
  expect_equal(report$estimates$estimate[report$estimates$metric == "tie"], 1)
})
