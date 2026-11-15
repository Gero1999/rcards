test_that("locales validate and fall back without changing user values", {
  expect_equal(supported_languages(),c("en","es","de","it"))
  expect_equal(console_language(),"en")
  expect_error(console_language("xx"),"Supported languages")
  expect_equal(ui("unknown %s", "it", "Alice"),"unknown Alice")
})

test_that("localized commands map to canonical actions without changing names", {
  for(language in c("es","de","it")) {
    legal <- list(actions=c("fold","call","raise"), min_raise_to=4,max_raise_to=100)
    controller <- console_player(language=language)
    token <- action_label("raise",language)
    expect_equal(console_decision(paste(token,10),legal,controller),list(action="raise",amount=10))
    expect_equal(console_decision("raise 10",legal,controller)$action,"raise")
    expect_equal(console_decision(action_label("quit",language),legal,controller)$action,"quit")
    expect_false(identical(ui("Your action: ",language),"Your action: "))
  }
})
