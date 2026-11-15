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

test_that("single captures take priority over sums and alternatives stay explicit", {
  cards <- list(card("5","Denari"),card("5","Coppe"),card("2","Spade"),card("3","Bastoni"))
  expect_equal(scopa_capture_options(cards,5),list(1L,2L))
  expect_equal(scopa_capture_options(cards[-(1:2)],5),list(c(1L,2L)))
  expect_equal(scopa_capture_options(list(),4),list(integer()))
  g <- new_scopa(seed=1)
  g$table_cards <- cards;g$players[[1]]$hand <- list(card("5","Spade"))
  expect_error(scopa_step(g,1),"Choose a capture")
  expect_equal(length(scopa_step(g,1,2)$captured[[1]]),2)
  expect_error(scopa_step(g,1,3),"capture option")
})

test_that("Scopa captures conserve all forty cards for every player count", {
  for(n in 2:4) for(seed in 1:10) {
    g <- new_scopa(paste0("P",seq_len(n)),seed=seed)
    turns <- 0L
    while(!g$hand_complete) {
      g <- scopa_step(g,1,1)
      expect_length(italian_card_ids(scopa_cards(g)),40)
      turns <- turns+1L
      if(turns>36L) stop("Deal did not terminate")
    }
    expect_equal(turns,36L)
    expect_length(g$stock,0)
    expect_true(all(vapply(g$players,function(p) !length(p$hand),logical(1))))
  }
})

test_that("the last capture gets leftovers but final-play captures earn no sweep", {
  g <- new_scopa(seed=1)
  g$stock <- list();g$table_cards <- list(card("5","Denari"))
  g$players[[1]]$hand <- list(card("5","Coppe"));g$players[[2]]$hand <- list()
  g <- scopa_step(g,1)
  expect_equal(g$sweeps,c(0L,0L))
  expect_length(g$captured[[1]],2)
  g <- new_scopa(seed=1);g$last_capturer<-2L
  remaining <- g$table_cards
  g <- scopa_close_deal(g)
  expect_identical(g$captured[[2]],remaining)
  expect_equal(g$sweeps,c(0L,0L))
})
