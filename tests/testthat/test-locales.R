test_that("locales validate and fall back without changing user values", {
  expect_true(all(c("en","es","de","it") %in% supported_languages()))
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

test_that("localized play translates errors and keeps names and canonical state", {
  h <- new_holdem(seed=1)
  for(language in c("es","de","it")) {
    io <- console_fixture(c("raise 0","q"))
    io$controller$language <- language
    result <- play(h,list(io$controller,passive_controller))
    expect_identical(result$game,h)
    expect_false(grepl("Please try again|Your cards|must be a finite",io$output()))
    expect_match(translate_card(card("A","Spades"),language)$display,
      switch(language,es="As",de="Ass",it="Asso"),fixed=TRUE)
  }
  for(entry in locale_catalog) expect_equal(sort(names(entry)),sort(setdiff(supported_languages(),"en")))
})

test_that("different console locales receive their own completion messages", {
  frames <- character()
  writer <- function(text) frames <<- c(frames,text)
  g <- holdem_action(new_holdem(seed=1),"fold")
  console_notify(g,list(console_player(write=writer,language="de"),console_player(write=writer,language="it")),"complete")
  expect_length(frames,2)
  expect_match(frames[1],"gewinnt")
  expect_match(frames[2],"vince")
})
