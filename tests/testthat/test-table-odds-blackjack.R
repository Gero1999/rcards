test_that("Blackjack comparison renders only legal actions and leaves the round intact", {
  skip_if_not_installed("shiny")
  game <- new_bj("8s 9h 8d 8c 2h")
  game <- blackjack_step(game, "hit")
  before <- game
  set.seed(17)
  rng <- .Random.seed
  dialog <- table_blackjack_odds_dialog(game_observation(game))
  rendered <- as.character(dialog)

  expect_identical(game, before)
  expect_identical(.Random.seed, rng)
  expect_match(rendered, "Expected net profit (chips)", fixed = TRUE)
  expect_match(rendered, "500 simulated rounds per action.", fixed = TRUE)
  expect_match(rendered, 'aria-labelledby="rcards-blackjack-odds-title"', fixed = TRUE)
  expect_match(rendered, '<th scope="row">stand</th>', fixed = TRUE)
  expect_false(grepl('<th scope="row">(split|double)</th>', rendered))
})

test_that("Blackjack comparison uses no dealer hole card or live shoe information", {
  skip_if_not_installed("shiny")
  game <- new_bj("8s 9h 8d 8c")
  before <- as.character(table_blackjack_odds_dialog(game_observation(game), "es"))
  game$dealer[[2L]] <- card("2", "Clubs")
  game$shoe <- rev(game$shoe)
  after <- as.character(table_blackjack_odds_dialog(game_observation(game), "es"))

  expect_identical(before, after)
  expect_match(after, '<th scope="row">dividir</th>', fixed = TRUE)
  expect_match(after, '<th scope="row">doblar</th>', fixed = TRUE)
})
