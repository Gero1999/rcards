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

test_that("Scopa scoring has known primiera weights and no tied majority point", {
  sevens <- lapply(c("Denari","Coppe","Spade","Bastoni"),function(s) card("7",s))
  expect_equal(scopa_primiera(sevens),84)
  expect_true(is.na(scopa_primiera(sevens[-1])))
  sixes <- lapply(c("Denari","Coppe","Spade","Bastoni"),function(s) card("6",s))
  score <- scopa_deal_score(list(sevens,sixes),1:2,c(2L,0L))
  # Card/coin totals tie. Sevens win settebello and primiera, plus two sweeps.
  expect_equal(score$points,c(4L,0L))
  score <- scopa_deal_score(list(sevens[1:2],sixes,sevens[3:4],list()),c(1L,2L,1L,2L),c(1L,0L,1L,0L))
  expect_equal(score$points,c(4L,0L))
  expect_equal(scopa_majority(c(20,20)),integer())
  expect_equal(scopa_majority(c(NA,NA)),integer())
})

test_that("Scopa matches settle whole deals and rotate dealer with scores retained", {
  g <- new_scopa(rules=scopa_rules(target=100),seed=1)
  while(!g$hand_complete) g <- scopa_step(g,1,1)
  next_game <- scopa_next_deal(g,seed=2)
  expect_equal(next_game$dealer,1)
  expect_equal(next_game$actor,2)
  expect_equal(next_game$hand_number,2)
  expect_equal(next_game$scores,g$scores)
  expect_length(italian_card_ids(scopa_cards(next_game)),40)
  expect_equal(next_game$sweeps,c(0L,0L))
  expect_error(scopa_next_deal(next_game),"Finish")
  # An equal leading score at the target cannot end a match.
  g <- new_scopa(rules=scopa_rules(target=1),seed=1)
  g$scores <- c(1,1);g$captured <- list(list(),list());g$sweeps <- c(0L,0L)
  expect_false(scopa_settle(g)$complete)
  g$scores <- c(2,1)
  expect_true(scopa_settle(g)$complete)
})

test_that("Scopa public protocol bots and matches support all player counts", {
  for(n in 2:4) {
    g <- scopa_game(paste0("P",seq_len(n)),rules=scopa_rules(target=3),seed=7)
    o <- game_observation(g)
    expect_null(o[["stock"]]);expect_null(o$players);expect_length(o$hole,3)
    expect_equal(o$hand_counts,rep(3L,n))
    result <- play(g,rep(list(scopa_bot()),n),until="game",seed=42)
    expect_true(result$game$complete)
    expect_true(max(result$game$scores)>=3)
    expect_length(italian_card_ids(scopa_cards(result$game)),40)
    expect_length(game_observation(result$game)$hole,0)
    expect_true(length(unique(result$game$winner))==1L)
  }
})

test_that("Scopa console selects capture options in every language and pauses safely", {
  for(language in supported_languages()) {
    g <- scopa_game(seed=3)
    io <- console_fixture(c(paste(action_label("play",language),"1 1"),"q"))
    io$controller$language <- language
    result <- play(g,list(io$controller,scopa_bot()))
    expect_equal(result$status,"quit")
    expect_equal(result$game$history[[1]]$player,1)
    expect_match(io$output(),"SCOPA")
    expect_length(italian_card_ids(scopa_cards(result$game)),40)
  }
  g <- scopa_game(seed=1)
  io <- console_fixture(c("play 99 1","q"))
  result <- play(g,list(io$controller,scopa_bot()))
  expect_identical(result$game,g)
  expect_error(game_action(g,list(action="hit")),"Illegal Scopa")
})

test_that("Scopa prompts for ambiguous captures and quitting keeps the state", {
  g <- scopa_game(seed=1)
  g$table_cards <- list(card("5","Denari"),card("5","Coppe"))
  g$players[[1]]$hand <- list(card("5","Spade"))
  io <- console_fixture(c("play","1","q"))
  result <- play(g,list(io$controller,scopa_bot()))
  expect_identical(result$game,g)
  expect_true(any(grepl("Capture option 1..2",io$prompts(),fixed=TRUE)))
})
