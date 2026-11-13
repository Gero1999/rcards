make_hand <- function(values, suits = rep("Spades", length(values))) {
  Map(card, values, suits)
}

test_that("hand ranking uses category and correct kickers", {
  wheel <- make_hand(c("A", "2", "3", "4", "5"))
  six <- make_hand(as.character(2:6))
  expect_gt(poker_hand_points(six), poker_hand_points(wheel))
  expect_gt(poker_hand_points(make_hand(c("A","A","K","Q","2"), c("Spades","Hearts","Clubs","Diamonds","Hearts"))),
            poker_hand_points(make_hand(c("K","K","A","Q","J"), c("Spades","Hearts","Clubs","Diamonds","Hearts"))))
  expect_error(poker_hand_points(rep(wheel[1], 5)), "Duplicate")
  seven <- c(six, list(card("A", "Hearts"), card("K", "Hearts")))
  expect_equal(poker_hand_points(seven), poker_hand_points(six))
})

test_that("eligible winners retain their original player index", {
  low <- make_hand(c("2","4","6","8","10"))
  high <- make_hand(c("10","J","Q","K","A"))
  g <- list(players = list(player(hand = low), player(hand = low), player(hand = high)),
            table = list(pots = list(list(amount = 17, eligible_players = c(2, 3)))))
  expect_equal(evaluate_poker_hands(g, c(2, 3)), 3)
  settled <- determine_poker_winner(g)
  expect_equal(settled$players[[3]]$points, 17)
  expect_equal(settled$table$pots[[1]]$amount, 0)
  expect_error(player_bet_poker(g, 1, -1), "bet")
})
