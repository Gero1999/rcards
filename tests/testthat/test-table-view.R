test_that("shared table projections keep a fixed viewer while other seats act", {
  games <- list(new_holdem(seed=1),mus_game(paste0("P",1:4),seed=1),blackjack_game(seed=2),
    scopa_game(seed=1),liar_game(seed=1))
  for(g in games) {
    view <- game_table_view(g,1)
    expect_equal(view$viewer,1)
    expect_null(view[["game"]]);expect_null(view$controllers)
    for(seat in view$seats[-1]) for(hand in seat$hands)
      expect_true(all(vapply(hand$cards,function(card) isTRUE(card$back),logical(1))))
  }
  g <- holdem_action(new_holdem(seed=1),"call")
  view <- game_table_view(g,1)
  expect_false(view$your_turn)
  expect_length(view$legal$actions,0)
  expect_identical(view$own[[1]]$cards,game_table_view(new_holdem(seed=1),1)$own[[1]]$cards)
})

test_that("table views reveal only authorized showdowns and challenged Liar batches", {
  fold <- holdem_action(new_holdem(seed=1),"fold")
  expect_true(all(vapply(game_table_view(fold,1)$seats[[2]]$hands[[1]]$cards,`[[`,logical(1),"back")))
  showdown <- finish_calling(new_holdem(seed=1))
  expect_false(any(vapply(game_table_view(showdown,1)$seats[[2]]$hands[[1]]$cards,`[[`,logical(1),"back")))
  g <- liar_game(seed=1)
  g <- game_action(g,list(action="claim",cards=1,amount="1"))
  view <- game_table_view(g,2)
  expect_identical(view$center[[1]]$cards,list(list(back=TRUE)))
  changed <- g;changed$last_claim$cards <- rev(g$players[[1]]$hand[1])
  expect_identical(game_table_view(changed,2),view)
})

test_that("adapters produce engine decisions for amounts selections and captures", {
  g <- new_holdem(seed=1);view <- game_table_view(g)
  next_game <- game_action(g,table_decision(view,"raise",amount=4))
  expect_equal(next_game$round_bets[1],4)
  expect_error(game_action(g,table_decision(view,"raise",amount=0)),"raise-to")
  g <- scopa_game(seed=1);view<-game_table_view(g)
  expect_error(table_decision(view,"play"),"hand position")
  expect_error(game_action(g,table_decision(view,"play",selected=1,capture=1)),NA)
})

test_that("session transitions are transactional and reject stale double clicks", {
  state <- table_session(new_holdem(seed=1))
  original <- state
  expect_error(table_apply(state,list(action="raise",amount=0)),"raise-to")
  expect_identical(state,original)
  advanced <- table_apply(state,list(action="call"))
  expect_error(table_apply(advanced,list(action="call"),version=0),"turn changed")
  set.seed(4);before<-.Random.seed
  advanced <- table_bot_step(advanced)
  expect_identical(.Random.seed,before)
  expect_gt(advanced$actions,state$actions)
  paused <- table_session(new_holdem(seed=1),seat=2,max_bot_actions=1)
  paused <- table_bot_step(paused)
  expect_lte(paused$bot_actions,1)
})
