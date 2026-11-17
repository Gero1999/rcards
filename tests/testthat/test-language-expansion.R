test_that("registered locales cover templates and preserve interpolation", {
  for(lang in setdiff(supported_languages(),"en")) for(key in names(locale_catalog)) {
    value <- unname(locale_catalog[[key]][lang])
    expect_true(length(value)==1 && !is.na(value) && nzchar(value))
    placeholders <- function(x) regmatches(x,gregexpr("%[sdif]",x))[[1]]
    expect_identical(placeholders(value),placeholders(key))
  }
})

test_that("new card locales preserve canonical state for every pack", {
  for(lang in intersect(c("pt","zh","qu"),supported_languages())) {
    for(x in list(card("A","Spades"),card("12","Oros"),card("8","Denari"))) {
      changed <- translate_card(x,lang)
      expect_equal(changed$value,x$value);expect_equal(changed$suit,x$suit)
      expect_equal(changed$symbol,x$symbol);expect_true(nzchar(changed$display))
    }
    legal <- list(actions=c("raise","call","fold"),min_raise_to=4,max_raise_to=100)
    expect_equal(console_decision(paste(action_label("raise",lang),10),legal,console_player(language=lang))$amount,10)
  }
})
