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

liar_decline <- function(g) {
  while(!g$complete && g$phase=="response") g <- liar_step(g,"pass")
  g
}

test_that("Mentiroso keeps its rank until a challenge and follows response order", {
  g <- new_liar(seed=3)
  rank <- g$players[[1]]$hand[[1]]$value
  g <- liar_step(g,"claim",1,rank)
  expect_equal(g$actor,2)
  expect_equal(g$responders,c(2L,3L))
  expect_error(liar_step(g,"claim",1,rank),"Illegal")
  g <- liar_decline(g)
  expect_equal(g$actor,2)
  expect_equal(g$rank,rank)
  other <- setdiff(liar_ranks(g$rules),rank)[1]
  expect_error(liar_step(g,"claim",1,other),"current declared rank")
})

test_that("challenges punish the liar or challenger and reveal only the latest batch", {
  for(lie in c(FALSE,TRUE)) {
    g <- new_liar(seed=3)
    old_rank <- g$players[[1]]$hand[[1]]$value
    g <- liar_decline(liar_step(g,"claim",1,old_rank))
    i <- g$actor
    values <- vapply(g$players[[i]]$hand,`[[`,"","value")
    position <- which(if(lie) values!=old_rank else values==old_rank)[1]
    if(is.na(position)) next
    batch <- g$players[[i]]$hand[position]
    g <- liar_step(g,"claim",position)
    expect_length(g$pile,2)
    challenger <- g$actor
    g <- liar_step(g,"challenge")
    expect_equal(g$last_reveal$caught,lie)
    expect_identical(g$last_reveal$cards,batch)
    expect_equal(g$last_reveal$pickup_count,2)
    expect_equal(g$actor,if(lie) i else challenger)
    expect_null(g$rank)
    expect_length(g$pile,0)
    expect_error(liar_assert_cards(g),NA)
  }
})

test_that("last-card claims cannot win before opponents have a response", {
  for(lie in c(FALSE,TRUE)) {
    g <- new_liar(seed=3)
    # Move excess actor cards to another hand while preserving all forty cards.
    g$players[[2]]$hand <- c(g$players[[2]]$hand,g$players[[1]]$hand[-1])
    g$players[[1]]$hand <- g$players[[1]]$hand[1]
    actual <- g$players[[1]]$hand[[1]]$value
    rank <- if(lie) setdiff(liar_ranks(g$rules),actual)[1] else actual
    pending <- liar_step(g,"claim",1,rank)
    expect_false(pending$complete)
    expect_equal(liar_decline(pending)$winner,1)
    challenged <- liar_step(pending,"challenge")
    expect_equal(challenged$complete,!lie)
    if(lie) expect_gt(length(challenged$players[[1]]$hand),0) else expect_equal(challenged$winner,1)
    expect_error(liar_assert_cards(challenged),NA)
  }
})
