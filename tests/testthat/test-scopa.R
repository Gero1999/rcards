scopa_cards <- function(g) c(g$stock,g$table_cards,
  unlist(lapply(g$players,`[[`,"hand"),recursive=FALSE),unlist(g$captured,recursive=FALSE))

test_that("Scopa deals and assigns teams for all supported player counts", {
  for(n in 2:4) {
    g <- new_scopa(paste0("P",seq_len(n)),seed=1)
    expect_equal(vapply(g$players,function(p) length(p$hand),0L),rep(3L,n))
    expect_length(g$table_cards,4)
    expect_length(g$stock,36-3*n)
    expect_length(unique(italian_card_ids(scopa_cards(g))),40)
    expect_equal(g$teams,if(n==4) c(1L,2L,1L,2L) else seq_len(n))
    expect_equal(g$actor,1)
  }
  expect_error(new_scopa("Solo"),"two three or four")
  expect_error(new_scopa(c("A","A")),"distinct")
  expect_error(new_scopa(pack=italian_deck()[1:39]),"complete")
})
