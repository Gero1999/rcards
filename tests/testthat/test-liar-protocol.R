test_that("Liar observations disclose no unrevealed cards or hidden truth flags", {
  g <- liar_game(seed=3)
  rank <- g$players[[1]]$hand[[1]]$value
  pending <- game_action(g,list(action="claim",amount=rank,cards=1))
  o <- game_observation(pending)
  expect_named(o$claim,c("player","rank","count"))
  expect_null(o[["pile"]]);expect_null(o[["last_claim"]]);expect_null(o$reveal)
  expect_named(o$history[[1]],c("action","player","rank","count"))
  expect_identical(o$hole,pending$players[[o$actor]]$hand)
  # Swap another player's hidden card with the face-down batch, maintaining counts.
  changed <- pending
  card <- changed$players[[1]]$hand[[1]]
  changed$players[[1]]$hand[[1]] <- changed$pile[[1]]
  changed$pile[[1]] <- card;changed$last_claim$cards <- list(card)
  expect_identical(game_observation(changed),o)
  finished <- game_action(pending,list(action="challenge"))
  expect_length(game_observation(finished)$reveal$cards,1)
})

test_that("localized Liar claims pause and resume without exposing private batches", {
  for(language in supported_languages()) {
    g <- liar_game(seed=3)
    rank <- g$players[[1]]$hand[[1]]$value
    io <- console_fixture(c(paste(action_label("claim",language),rank,"1")))
    io$controller$language <- language
    quit <- function(o) list(action="quit")
    result <- play(g,c(list(io$controller),rep(list(quit),2)))
    expect_equal(result$status,"quit")
    expect_equal(result$game$phase,"response")
    expect_length(result$game$pile,1)
    expect_match(io$output(),"MENTIROSO")
  }
  io <- console_fixture(c("claim 1 999","q"))
  g <- liar_game(seed=3)
  result <- play(g,c(list(io$controller),rep(list(function(o) list(action="quit")),2)))
  expect_identical(result$game,g)
})
