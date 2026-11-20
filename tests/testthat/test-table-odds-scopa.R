test_that("Scopa move comparison identifies each legal capture and pass-through card", {
  skip_if_not_installed("shiny")
  prefix <- list(card("5", "Spade"), card("1", "Denari"), card("4", "Denari"),
    card("2", "Denari"), card("10", "Bastoni"), card("8", "Denari"),
    card("5", "Denari"), card("5", "Coppe"), card("2", "Spade"), card("3", "Bastoni"))
  pack <- as_card_list(italian_deck())
  pack <- c(prefix, pack[!italian_card_ids(pack) %in% italian_card_ids(prefix)])
  game <- scopa_game(pack = pack)
  dialog <- as.character(table_scopa_odds_dialog(game_observation(game)))

  expect_match(dialog, 'aria-labelledby="rcards-scopa-odds-title"', fixed = TRUE)
  expect_match(dialog, "Chance to finish the deal ahead", fixed = TRUE)
  expect_match(dialog, "95% simulation interval (points)", fixed = TRUE)
  expect_length(regmatches(dialog, gregexpr('<th scope="row">', dialog, fixed = TRUE))[[1L]], 6L)
  expect_match(dialog, "#1 5 of Swords", fixed = TRUE)
  expect_match(dialog, "#1 5 of Coins", fixed = TRUE)
  expect_match(dialog, "#2 5 of Cups", fixed = TRUE)
  expect_match(dialog, "#3 King of Batons", fixed = TRUE)
  expect_match(dialog, "No capture", fixed = TRUE)
})

test_that("Scopa comparison preserves RNG and depends only on visible information", {
  skip_if_not_installed("shiny")
  game <- scopa_game(seed = 1)
  original <- game
  set.seed(17)
  rng <- .Random.seed
  dialog <- as.character(table_scopa_odds_dialog(game_observation(game)))

  expect_identical(game, original)
  expect_identical(.Random.seed, rng)

  game$players[[2L]]$hand <- rev(game$players[[2L]]$hand)
  game$stock <- rev(game$stock)
  expect_identical(as.character(table_scopa_odds_dialog(game_observation(game))), dialog)
})
