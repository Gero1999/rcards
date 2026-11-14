test_that("console players complete hands through the same protocol as policies", {
  io <- console_fixture(c("call", "check", "check", "check"))
  h <- new_holdem(seed = 17)
  result <- play(h, list(io$controller, passive_controller))
  expect_equal(result$status, "complete")
  expect_true(game_complete(result$game))
  expect_equal(sum(vapply(result$game$players, `[[`, 0, "points")), 200)
  expect_match(io$output(), "Your cards")
  expect_match(io$output(), "To call")
  expect_match(io$output(), "wins|split")
  expect_output(print(result), "Play status: complete")
  expect_equal(result$game, finish_calling(h))
})

test_that("a console controller can play an existing statistical bot", {
  frame <- ""; count <- 0L
  reader <- function(prompt) {
    count <<- count + 1L
    if (grepl("Actions: .*check", frame)) "check" else "call"
  }
  human <- console_player(read = reader, write = function(text) frame <<- text)
  result <- play(new_holdem(seed = 19), list(human, poker_bot(equity_samples = 8, seed = 4)))
  expect_equal(result$status, "complete")
  expect_gt(count, 0)
  expect_gt(result$controllers[[2]]$decisions, 0)
})

test_that("invalid console inputs retry without mutating the state", {
  h <- new_holdem(seed = 1)
  io <- console_fixture(c("invalid", "check", "raise 0", "q"))
  result <- play(h, list(io$controller, passive_controller))
  expect_equal(result$status, "quit")
  expect_equal(result$actions, 0)
  expect_identical(result$game, h)
  expect_match(io$output(), "Please try again")
  bounded <- console_fixture(c("invalid", "raise 0"))
  result <- play(h, list(bounded$controller, passive_controller), max_invalid = 2)
  expect_equal(result$status, "input_limit")
  expect_identical(result$game, h)
  expect_match(result$last_error, "raise-to")
})

test_that("menus, prompted amounts and discard commands use engine validation", {
  io <- console_fixture(c("3", "4", "q"))
  result <- play(new_holdem(seed = 1), list(io$controller, passive_controller))
  expect_equal(result$status, "quit")
  expect_equal(result$game$history[[1]]$action, "raise")
  expect_equal(result$game$history[[1]]$amount, 4)
  expect_true(any(grepl("Total amount", io$prompts())))
  g <- new_mus(seed = 1)
  for (i in 1:4) g <- mus_action(g, "mus")
  io <- console_fixture(c("discard 1 1", "discard 1,3"))
  quit <- function(o) list(action = "quit")
  result <- play(g, c(list(io$controller), rep(list(quit), 3)))
  expect_equal(result$status, "quit")
  expect_equal(result$actions, 1)
  expect_length(result$game$players[[1]]$hand, 4)
  expect_match(io$output(), "Please try again")
  expect_equal(tail(result$game$history, 1)[[1]]$cards, c(1, 3))
})

test_that("EOF, interruption and quit during amount input are resumable", {
  h <- new_holdem(seed = 1)
  for (lines in list(list(), list("raise", "q"), list(NA_character_))) {
    io <- console_fixture(lines)
    result <- play(h, list(io$controller, passive_controller))
    expect_equal(result$status, "quit")
    expect_identical(result$game, h)
  }
  interrupt <- structure(list(message = "interrupted", call = NULL), class = c("interrupt", "condition"))
  io <- console_fixture(list(interrupt))
  result <- play(h, list(io$controller, passive_controller))
  expect_equal(result$status, "interrupted")
  expect_identical(result$game, h)
  result$controllers[[1]] <- passive_controller
  expect_equal(play(result$game, result$controllers)$status, "complete")
})

test_that("stateful controllers retain updates across play and simulation", {
  policy <- policy_player(function(o, state) {
    list(decision = passive_controller(o), state = state + 1L)
  }, state = 0L)
  h <- new_holdem(seed = 1)
  limited <- play(h, rep(list(policy), 2), max_actions = 1)
  expect_equal(limited$status, "action_limit")
  expect_equal(limited$actions, 1)
  result <- play(limited$game, limited$controllers)
  expect_equal(result$status, "complete")
  expect_equal(sum(vapply(result$controllers, `[[`, 0L, "state")), result$actions + limited$actions)
  players <- list(player("A", points = 20), player("B", points = 20))
  simulation <- simulate_poker_game(players, rep(list(policy), 2), hands = 2, seed = 1)
  expect_true(all(vapply(simulation$bots, `[[`, 0L, "state") > 1L))
  expect_error(play(h, list(function(o) list(action = "check"), passive_controller)), "Illegal")
  expect_error(play(h, list(poker_bot())), "one supported controller")
  expect_error(choose_action(policy_player(function(o, state) list(action = "call")), game_observation(h)), "decision and state")
})

test_that("the shared loop can finish Mus hands or a whole configured game", {
  controllers <- rep(list(passive_mus_controller), 4)
  g <- new_mus(rules = mus_rules(4, 30), seed = 12)
  hand <- play(g, controllers)
  expect_equal(hand$status, "hand_complete")
  expect_true(game_hand_complete(hand$game))
  expect_false(game_complete(hand$game))
  expect_equal(game_next_hand(hand$game)$hand_number, 2)
  set.seed(42); before <- .Random.seed
  whole <- play(g, controllers, until = "game", seed = 42)
  expect_equal(whole$status, "complete")
  expect_true(game_complete(whole$game))
  expect_true(max(whole$result$scores) >= 30)
  expect_identical(.Random.seed, before)
  expect_identical(whole, play(g, controllers, until = "game", seed = 42))
  expect_error(choose_action(poker_bot(), game_observation(g)), "Hold'em only")
})

test_that("four console seats use the same Mus loop", {
  frame <- ""
  reader <- function(prompt) if (grepl("Actions: .*cut", frame)) "cut" else "pass"
  human <- console_player(read = reader, write = function(text) frame <<- text, language = "es")
  result <- play(new_mus(seed = 4), rep(list(human), 4))
  expect_equal(result$status, "hand_complete")
  expect_match(frame, "Scores")
  expect_equal(result$game, finish_mus_passing(new_mus(seed = 4)))
})

test_that("quitting retains stateful controller updates for resumption", {
  policy <- policy_player(function(o, state) {
    decision <- if (state == 0L) list(action = "quit") else passive_controller(o)
    list(decision = decision, state = state + 1L)
  }, 0L)
  h <- new_holdem(seed = 1)
  stopped <- play(h, list(policy, passive_controller))
  expect_identical(stopped$game, h)
  expect_equal(stopped$controllers[[1]]$state, 1L)
  resumed <- play(stopped$game, stopped$controllers)
  expect_equal(resumed$status, "complete")
  expect_gt(resumed$controllers[[1]]$state, 1L)
})

test_that("console input supports standing pat and fails promptly in batch mode", {
  g <- new_mus(seed = 1)
  for (i in 1:4) g <- mus_action(g, "mus")
  io <- console_fixture(c("discard none", "q"))
  played <- play(g, rep(list(io$controller), 4))
  expect_equal(played$status, "quit")
  expect_equal(played$actions, 1L)
  expect_identical(played$game$players[[1]]$hand, g$players[[1]]$hand)
  if (!interactive()) {
    expect_error(choose_action(console_player(write = function(text) NULL),
                               game_observation(new_holdem(seed = 1))), "interactive R session")
  }
})
