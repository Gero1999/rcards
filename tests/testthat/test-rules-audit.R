# Test inputs and expected winners are hand-authored from standard poker rules.
# No external rankings, generated lookup tables, or additional packages are used.
audit_hand <- function(text) {
  suits <- c(s = "Spades", h = "Hearts", d = "Diamonds", c = "Clubs")
  lapply(strsplit(text, " ")[[1]], function(code) {
    value <- substr(code, 1, 1)
    if (value == "T") value <- "10"
    card(value, unname(suits[substr(code, 2, 2)]))
  })
}

expect_audit_winners <- function(cases) {
  for (example in cases) {
    winner <- poker_hand_points(audit_hand(example[["winner"]]))
    loser <- poker_hand_points(audit_hand(example[["loser"]]))
    expect_true(winner > loser, info = example[["reason"]])
  }
}

test_that("each poker category beats the next lower category", {
  expect_audit_winners(list(
    c(reason = "Straight flush beats four of a kind",
      winner = "5h 6h 7h 8h 9h", loser = "As Ah Ad Ac Kh"),
    c(reason = "Four of a kind beats a full house",
      winner = "2s 2h 2d 2c 3h", loser = "As Ah Ad Kc Kh"),
    c(reason = "Full house beats a flush",
      winner = "2s 2h 2d 3c 3h", loser = "Ah Jh 9h 6h 3h"),
    c(reason = "Flush beats a straight",
      winner = "2h 4h 6h 8h Th", loser = "Ts Jh Qd Kc As"),
    c(reason = "Straight beats three of a kind",
      winner = "2s 3h 4d 5c 6s", loser = "As Ah Ad Kc Qh"),
    c(reason = "Three of a kind beats two pair",
      winner = "2s 2h 2d 3c 4h", loser = "As Ah Kd Kc Qh"),
    c(reason = "Two pair beats one pair",
      winner = "2s 2h 3d 3c 4h", loser = "As Ah Kd Qc Jh"),
    c(reason = "One pair beats high card",
      winner = "2s 2h 3d 4c 6h", loser = "As Kh Qd Jc 9h")
  ))
})

test_that("poker ties within a category compare ranks and kickers in order", {
  expect_audit_winners(list(
    c(reason = "Royal flush is the highest straight flush",
      winner = "Th Jh Qh Kh Ah", loser = "9s Ts Js Qs Ks"),
    c(reason = "Six-high straight flush beats an ace-low straight flush",
      winner = "2h 3h 4h 5h 6h", loser = "As 2s 3s 4s 5s"),
    c(reason = "Quad rank is compared before its kicker",
      winner = "Ks Kh Kd Kc 2h", loser = "Qs Qh Qd Qc Ah"),
    c(reason = "Equal quads use the remaining kicker",
      winner = "9s 9h 9d 9c As", loser = "9s 9h 9d 9c Kh"),
    c(reason = "Full house compares trips before its pair",
      winner = "Ks Kh Kd 2c 2h", loser = "Qs Qh Qd Ac Ah"),
    c(reason = "Equal full-house trips compare the pair",
      winner = "Qs Qh Qd Ac Ah", loser = "Qs Qh Qd Kc Kh"),
    c(reason = "Flush compares the highest differing card",
      winner = "Ah Jh 9h 6h 3h", loser = "Ks Js 9s 6s 3s"),
    c(reason = "Flush can be decided by its fifth card",
      winner = "Ah Jh 9h 6h 4h", loser = "As Js 9s 6s 3s"),
    c(reason = "Six-high straight beats the five-high wheel",
      winner = "2s 3h 4d 5c 6s", loser = "As 2h 3d 4c 5s"),
    c(reason = "Ace cannot wrap a straight through king and two",
      winner = "2s 2h 4d 7c 9h", loser = "Ks Ah 2d 3c 4h"),
    c(reason = "Trip rank is compared before kickers",
      winner = "Ks Kh Kd 2c 3h", loser = "Qs Qh Qd Ac Jh"),
    c(reason = "Equal trips compare their highest kicker",
      winner = "9s 9h 9d Ac 2h", loser = "9s 9h 9d Kc Qh"),
    c(reason = "Equal trips and first kicker compare the second kicker",
      winner = "9s 9h 9d Ac Kh", loser = "9s 9h 9d Ac Qh"),
    c(reason = "Two pair compares the higher pair first",
      winner = "As Ah 2d 2c 3h", loser = "Ks Kh Qd Qc Ah"),
    c(reason = "Equal top pairs compare the lower pair",
      winner = "As Ah 3d 3c 2h", loser = "As Ah 2d 2c Kh"),
    c(reason = "Equal two pairs compare the fifth card",
      winner = "As Ah Kd Kc Qh", loser = "As Ah Kd Kc Jh"),
    c(reason = "Pair rank is compared before kickers",
      winner = "Ks Kh 2d 3c 4h", loser = "Qs Qh Ad Kc Jh"),
    c(reason = "Equal pairs compare their first kicker",
      winner = "As Ah Kc Qd 2s", loser = "Ac Ad Qh Js 9c"),
    c(reason = "Equal pairs and first kicker compare the second kicker",
      winner = "As Ah Kc Qd 2s", loser = "Ac Ad Kh Js 9c"),
    c(reason = "Equal pairs and two kickers compare the third kicker",
      winner = "As Ah Kc Qd Js", loser = "Ac Ad Kh Qs Tc"),
    c(reason = "High card compares the top card first",
      winner = "As Jh 9d 6c 3h", loser = "Ks Qh Jd 9c 8h"),
    c(reason = "High card can be decided by the fifth card",
      winner = "As Jh 9d 6c 4h", loser = "Ac Jd 9h 6s 3c")
  ))
})

test_that("six and seven cards use the explicitly expected best five", {
  cases <- list(
    c(reason = "Six cards: an off-suit ace does not replace a flush card",
      hand = "2h 5h 7h 9h Jh As", best = "2h 5h 7h 9h Jh"),
    c(reason = "Six cards: two triplets form the higher full house",
      hand = "As Ah Ad Kc Kh Kd", best = "As Ah Ad Kc Kh"),
    c(reason = "Six cards: three pairs use the top two and highest remaining card",
      hand = "As Ah Kc Kd Qs Qh", best = "As Ah Kc Kd Qs"),
    c(reason = "Seven cards: six suited cards use the highest five",
      hand = "Ah Kh 9h 7h 4h 2h Qc", best = "Ah Kh 9h 7h 4h"),
    c(reason = "Seven cards: choose seven-high rather than the wheel",
      hand = "As 2h 3d 4c 5s 6h 7d", best = "3d 4c 5s 6h 7d"),
    c(reason = "Seven cards: suited six-high beats a higher mixed-suit straight",
      hand = "2h 3h 4h 5h 6h 7c 8d", best = "2h 3h 4h 5h 6h"),
    c(reason = "Seven cards: ace can complete a ten-to-ace straight",
      hand = "As Kh Qd Jc Ts 2h 3d", best = "As Kh Qd Jc Ts"),
    c(reason = "Seven cards: two triplets still use the higher trips",
      hand = "As Ah Ad Kc Kh Kd 2s", best = "As Ah Ad Kc Kh"),
    c(reason = "Seven cards: a triplet and two pairs use the higher pair",
      hand = "As Ah Ad Kc Kh Qs Qh", best = "As Ah Ad Kc Kh"),
    c(reason = "Seven cards: the third pair supplies the kicker",
      hand = "As Ah Kc Kd Qs Qh Jd", best = "As Ah Kc Kd Qs"),
    c(reason = "Seven cards: quads use the highest side card",
      hand = "9s 9h 9d 9c 2s Kh Ad", best = "9s 9h 9d 9c Ad")
  )
  for (example in cases) {
    expect_equal(poker_hand_points(audit_hand(example[["hand"]])),
                 poker_hand_points(audit_hand(example[["best"]])),
                 info = example[["reason"]])
  }
})

test_that("equal best five tie regardless of suits or unused hole cards", {
  expect_equal(poker_hand_points(audit_hand("As Kh 9d 6c 3h")),
               poker_hand_points(audit_hand("Ac Kd 9h 6s 3c")))
  expect_equal(poker_hand_points(audit_hand("As Ah Kd Qc Jh")),
               poker_hand_points(audit_hand("Ac Ad Kh Qs Jc")))
  # The board's royal flush plays for both seats, even against a pocket pair.
  game <- list(players = list(player(hand = audit_hand("2h 3d")),
                              player(hand = audit_hand("9c 9h"))),
               board = audit_hand("Ts Js Qs Ks As"))
  expect_equal(evaluate_poker_hands(game), c(1L, 2L))
})

test_that("accepted Mus wagers wait for showdown and do not preempt a later ordago", {
  g <- mus_action(new_mus(seed=3),"cut")
  g$scores <- c(39,39)
  g <- mus_action(g,"bet",amount=2)
  g <- mus_action(g,"accept")
  expect_equal(g$phase,"chica")
  expect_equal(g$scores,c(39,39))
  expect_false(g$complete)
  expect_false(g$revealed)
  g <- mus_action(g,"ordago")
  expected <- mus_winner(g)
  g <- mus_action(g,"accept")
  expect_true(g$complete)
  expect_equal(g$winner,expected)
  expect_true(g$revealed)
  expect_equal(length(g$results),1)
  expect_equal(g$results[[1]]$lance,"chica")
})

test_that("Mus refusal points are immediate but intrinsic points wait", {
  g <- mus_action(new_mus(seed=3),"cut")
  g <- mus_action(g,"bet",amount=2)
  team <- g$bet_team
  g <- mus_action(g,"reject")
  expect_equal(g$scores[team],1)
  expect_false(g$revealed)
  finished <- finish_mus_passing(g)
  expect_true(finished$hand_complete)
  expect_true(finished$revealed)
  # Recorded settlements follow the lance order even after a refusal.
  lances <- vapply(finished$results,`[[`,"","lance")
  expect_true(all(diff(match(lances,c("grande","chica","pares","juego","punto")))>=0))
})

test_that("Mus hand-end settlement stops at the first game-winning lance", {
  g <- mus_action(new_mus(seed=3),"cut")
  g$scores <- c(39,39)
  finished <- finish_mus_passing(g)
  expect_true(finished$complete)
  expect_equal(length(finished$results),1)
  expect_equal(finished$results[[1]]$lance,"grande")
})
