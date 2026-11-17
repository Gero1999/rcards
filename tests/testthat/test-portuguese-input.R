test_that("Portuguese stand-pat input accepts nenhuma", {
  legal <- list(actions="discard",card_positions=1:4)
  decision <- console_decision("descartar nenhuma",legal,console_player(language="pt"))
  expect_identical(decision,list(action="discard",cards=integer()))
})
