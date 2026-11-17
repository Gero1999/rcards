test_that("registered locales cover templates and preserve interpolation", {
  for(lang in setdiff(supported_languages(),"en")) for(key in names(locale_catalog)) {
    value <- unname(locale_catalog[[key]][lang])
    expect_true(length(value)==1 && !is.na(value) && nzchar(value))
    placeholders <- function(x) regmatches(x,gregexpr("%[sdif]",x))[[1]]
    expect_identical(placeholders(value),placeholders(key))
  }
})

test_that("new card locales preserve canonical state for every pack", {
  for(lang in intersect(c("pt","zh","zh-Hant","qu"),supported_languages())) {
    for(x in list(card("A","Spades"),card("12","Oros"),card("8","Denari"))) {
      changed <- translate_card(x,lang)
      expect_equal(changed$value,x$value);expect_equal(changed$suit,x$suit)
      expect_equal(changed$symbol,x$symbol);expect_true(nzchar(changed$display))
    }
    legal <- list(actions=c("raise","call","fold"),min_raise_to=4,max_raise_to=100)
    expect_equal(console_decision(paste(action_label("raise",lang),10),legal,console_player(language=lang))$amount,10)
  }
})

test_that("Mandarin scripts are distinct and positional card labels format correctly", {
  if(!"zh-Hant" %in% supported_languages()) skip("Traditional Mandarin not installed")
  expect_equal(ui("Your cards","zh"),"\u4f60\u7684\u624b\u724c")
  expect_equal(ui("New match","zh-Hant"),"\u65b0\u6bd4\u8cfd")
  expect_match(translate_card(card("A","Spades"),"zh")$display,"\u9ed1\u6843A",fixed=TRUE)
  expect_equal(console_action("\u68c4\u724c","fold","zh-Hant"),"fold")
})
