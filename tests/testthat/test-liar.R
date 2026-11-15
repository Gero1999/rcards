test_that("Mentiroso deals complete configurable packs and preserves seeded RNG", {
  set.seed(42);before <- .Random.seed
  for(style in c("spanish","french")) for(n in 2:8) {
    g <- new_liar(paste0("P",seq_len(n)),rules=liar_rules(style),seed=1)
    counts <- vapply(g$players,function(p) length(p$hand),0L)
    expect_lte(max(counts)-min(counts),1)
    expect_equal(g$actor,1)
    expect_error(liar_assert_cards(g),NA)
  }
  expect_identical(.Random.seed,before)
  expect_error(new_liar("Solo"),"two through eight")
  expect_error(new_liar(c("A","A")),"distinct")
  expect_error(liar_rules(max_cards=5),"max_cards")
  expect_error(new_liar(pack=as_card_list(mus_pack())[1:39]),"complete")
})
