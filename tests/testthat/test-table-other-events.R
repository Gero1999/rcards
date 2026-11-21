test_that("Blackjack announces naturals only after settlement", {
  players <- list(player("You", points = 100), player("Bot", points = 100))
  game <- blackjack_game(players, rules = blackjack_rules(decks = 1L),
    pack = blackjack_pack("As 8h 9d Ts 8d 8s"))
  expect_false(game_hand_complete(game))
  expect_null(table_other_event(game, 1L))
  expect_null(table_other_event(game, 2L))
  settled <- blackjack_step(game, "stand")
  winner <- table_other_event(settled, 1L)
  expect_identical(winner$title, "Blackjack!")
  expect_identical(winner$tone, "win")
  expect_identical(winner$detail[[1L]]$key, "You have blackjack.")
  expect_identical(winner$detail[[2L]]$args, list("+15"))
  expect_identical(table_other_event(settled, 2L)$title, "You lost the round")

  dealer <- table_other_event(new_bj("8s Ah 9d Tc"), 1L)
  expect_identical(dealer$title, "Blackjack!")
  expect_identical(dealer$tone, "neutral")
  expect_identical(dealer$detail[[1L]]$key, "The dealer has blackjack.")
  expect_identical(dealer$detail[[2L]]$args, list("-10"))

  both <- table_other_event(new_bj("As Ah Ts Tc"), 1L)
  expect_identical(both$detail[[1L]]$key, "You and the dealer have blackjack.")
  expect_identical(both$detail[[2L]]$args, list("0"))
  expect_identical(both$tone, "neutral")
})

test_that("Blackjack round events use net stakes and distinguish ordinary 21", {
  split <- blackjack_step(new_bj("As 9h Ad 8c Ts Jd"), "split")
  split_event <- table_other_event(split, 1L)
  expect_identical(split_event$title, "You won the round!")
  expect_identical(split_event$detail[[1L]]$args, list("+20"))
  expect_identical(split_event, table_other_event(split, 1L))

  dealer_21 <- blackjack_step(new_bj("Ts 9h 8d 7c 5h"), "stand")
  expect_identical(table_other_event(dealer_21, 1L)$title, "You lost the round")
  push <- blackjack_step(new_bj("Ts 9h 8d 9c"), "stand")
  expect_identical(table_other_event(push, 1L)$title, "You broke even")
})

test_that("Scopa sweep events recognize scored sweeps and expire on the next move", {
  prefix <- list(card("10", "Bastoni"), card("6", "Bastoni"), card("9", "Bastoni"),
    card("7", "Bastoni"), card("8", "Bastoni"), card("6", "Coppe"),
    card("1", "Denari"), card("2", "Denari"), card("3", "Denari"), card("4", "Denari"))
  pack <- as_card_list(italian_deck())
  game <- scopa_game(pack = c(prefix, pack[!italian_card_ids(pack) %in% italian_card_ids(prefix)]))
  expect_null(table_other_event(game, 1L))
  swept <- scopa_step(game, 1L)
  expect_identical(table_other_event(swept, 1L)$title, "Scopa!")
  expect_identical(table_other_event(swept, 1L)$tone, "win")
  expect_identical(table_other_event(swept, 2L)$tone, "neutral")
  expect_identical(table_other_event(swept, 2L)$detail[[1L]]$args, list("You"))
  expect_identical(table_other_event(swept, 2L)$key, table_other_event(swept, 1L)$key)
  expect_null(table_other_event(scopa_step(swept, 1L), 1L))
})

test_that("a final Scopa clearing has no sweep event but match settlement can announce a winner", {
  game <- scopa_game(rules = scopa_rules(100L), seed = 24L)
  for (turn in seq_len(35L)) game <- scopa_step(game, 1L, 1L)
  settled <- scopa_step(game, 1L, 1L)
  expect_equal(length(game$table_cards), 1L)
  expect_equal(length(settled$history[[36L]]$taken), 1L)
  expect_false(settled$history[[36L]]$sweep)
  expect_true(game_hand_complete(settled))
  expect_null(table_other_event(settled, 1L))

  game$rules <- scopa_rules(1L)
  match <- scopa_step(game, 1L, 1L)
  expect_identical(table_other_event(match, 1L)$title, "You won the match!")
  expect_identical(table_other_event(match, 1L)$detail[[1L]]$args, list("2 \u2013 1"))
  expect_identical(table_other_event(match, 2L)$title_args, list("You"))
})

test_that("Mus match announcements use settled scores without revealing hidden comparison winners", {
  game <- mus_action(new_mus(seed = 3), "cut")
  game$scores <- c(39, 39)
  offer <- mus_action(game, "bet", amount = 2L)
  expect_null(table_other_event(mus_action(offer, "accept"), 1L))
  settled <- mus_action(offer, "reject")
  expect_false(settled$revealed)
  expect_identical(table_other_event(settled, 1L)$title, "You won the match!")
  expect_identical(table_other_event(settled, 3L)$tone, "win")
  expect_identical(table_other_event(settled, 2L)$title_args, list("A & C"))
  expect_identical(table_other_event(settled, 2L)$detail[[1L]]$args, list("40 \u2013 39"))
})

test_that("Mentiroso announces only confirmed challenges and completed deals", {
  game <- liar_game(c("A", "B", "C"), seed = 3L)
  rank <- game$players[[1L]]$hand[[1L]]$value
  truthful <- liar_step(game, "claim", 1L, rank)
  expect_null(table_other_event(liar_step(truthful, "challenge"), 2L))
  wrong <- setdiff(liar_ranks(game$rules), rank)[1L]
  pending <- liar_step(game, "claim", 1L, wrong)
  expect_null(table_other_event(pending, 2L))
  caught <- liar_step(pending, "challenge")
  expect_identical(table_other_event(caught, 2L)$title, "Bluff caught!")
  expect_identical(table_other_event(caught, 2L)$detail[[1L]]$args, list("A"))
  expect_identical(table_other_event(caught, 1L)$detail[[1L]]$key, "%s caught your bluff.")
  expect_identical(table_other_event(caught, 3L)$detail[[1L]]$args, list("B", "A"))
  expect_null(table_other_event(liar_step(caught, "claim", 1L, rank), 2L))

  game$players[[2L]]$hand <- c(game$players[[2L]]$hand, game$players[[1L]]$hand[-1L])
  game$players[[1L]]$hand <- game$players[[1L]]$hand[1L]
  pending <- liar_step(game, "claim", 1L, rank)
  expect_null(table_other_event(pending, 1L))
  won <- liar_decline(pending)
  expect_identical(table_other_event(won, 1L)$title, "You won the deal!")
  expect_identical(table_other_event(won, 2L)$title_args, list("A"))
})
