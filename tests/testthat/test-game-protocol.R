test_that("both engines implement the game protocol without exposing full state", {
  h <- new_holdem(seed = 4)
  m <- new_mus(seed = 4)
  for (g in list(h, m)) {
    expect_s3_class(g, "game")
    expect_false(game_complete(g))
    expect_false(game_hand_complete(g))
    o <- game_observation(g)
    expect_equal(o$actor, game_legal_actions(g)$actor)
    expect_identical(o$hole, g$players[[o$actor]]$hand)
    expect_false(any(c("players", "deck", "discard", "burned") %in% names(o)))
    expect_false(any(c("players", "deck", "discard", "hole") %in% names(game_result(g))))
    other <- if (o$actor == 1L) 2L else 1L
    expect_identical(game_observation(g, other)$hole, g$players[[other]]$hand)
    expect_error(game_observation(g, 0), "seat")
  }
  expect_error(game_complete(game()), "completion method")
  expect_error(play(game(), list()), "completion method")
  expect_error(game_action(h, list(action = "call", secret = 1)), "Decision")
  expect_error(game_action(h, list(action = "call", cards = 1)), "discard")
  expect_equal(game_action(h, list(action = "call")), holdem_action(h, "call"))
  expect_equal(game_action(m, list(action = "cut")), mus_action(m, "cut"))
})

test_that("Mus observations redact discards and cannot reveal opponents' hands", {
  g <- new_mus(seed = 1)
  before <- mus_observation(g)
  altered <- g
  altered$players[[2]]$hand <- rev(altered$players[[2]]$hand)
  expect_identical(mus_observation(altered), before)
  for (i in 1:4) g <- mus_action(g, "mus")
  g <- mus_action(g, "discard", cards = c(1, 3))
  view <- mus_observation(g)
  event <- view$history[[5]]
  expect_equal(event$discard_count, 2)
  expect_false("cards" %in% names(event))
  expect_null(before$eligible)
  expect_equal(game_legal_actions(g)$card_positions, 1:4)
  g <- new_mus(seed = 1)
  g <- mus_action(g, "cut")
  expect_equal(game_legal_actions(g)$min_stake, 2)
  expect_true(all(c("hole", "scores", "rules", "history") %in% names(mus_observation(g))))
})

test_that("public observations and printing stay safe at completion", {
  h <- holdem_action(new_holdem(seed = 2), "fold")
  expect_true(game_complete(h))
  expect_length(game_observation(h)$hole, 0)
  expect_length(game_legal_actions(h)$actions, 0)
  expect_output(print(h), "Stacks")
  expect_output(print(game()), "Game")
  expect_error(game_next_hand(h), "cannot advance")
})
