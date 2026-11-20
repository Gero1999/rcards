test_that("Spanish and Italian card faces share consistent suit artwork", {
  skip_if_not_installed("shiny")
  spanish <- c(coin = "Oros", cup = "Copas", sword = "Espadas", baton = "Bastos")
  italian <- c(coin = "Denari", cup = "Coppe", sword = "Spade", baton = "Bastoni")

  for (icon in names(spanish)) {
    spanish_card <- card("11", spanish[[icon]])
    italian_card <- card("9", italian[[icon]])
    mark <- paste0("rc-suit-mark rc-suit-", icon)
    expect_identical(table_suit_mark(spanish_card)$attribs$class, mark)
    expect_identical(table_suit_mark(italian_card)$attribs$class, mark)
  }
})

test_that("card faces preserve the printed rank and speak the localized name", {
  skip_if_not_installed("shiny")
  scopa <- list(back = FALSE, value = "9", suit = "Spade", symbol = "S",
                label = "Cavallo di Spade S")
  rendered <- table_card_tag(scopa, 1, "hand", 0, language = "it")
  html <- as.character(rendered)
  expect_match(html, 'class="rc-rank" aria-hidden="true">\\s*9', perl = TRUE)
  expect_match(html, 'rc-suit-mark rc-suit-sword', fixed = TRUE)
  expect_identical(rendered$attribs[["aria-label"]], "1: Cavallo di Spade")

  french <- card("Q", "Hearts")
  expect_identical(table_suit_mark(french)$children[[1]], french$symbol)
})
