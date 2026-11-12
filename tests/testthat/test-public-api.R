test_that("foundation functions are exported", {
  exports <- getNamespaceExports("rcards")
  expect_true(all(c("card", "deck", "player", "table", "game", "shuffle",
                    "discard_card", "take_cards", "leave_cards") %in% exports))
})
