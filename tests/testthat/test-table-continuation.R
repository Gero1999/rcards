test_that("folding ends a poker hand but retains the graphical table", {
  initial <- table_session(new_holdem(seed=1))
  folded <- table_apply(initial,list(action="fold"))
  expect_true(game_table_view(folded$game)$can_next)
  expect_equal(holdem_stacks(folded$game),c(99,101))
  set.seed(17);rng <- .Random.seed
  next_state <- table_next(folded,version=1)
  expect_identical(.Random.seed,rng)
  expect_equal(next_state$game$initial_stacks,c(99,101))
  expect_equal(next_state$game$dealer,2)
  expect_identical(next_state$controllers,folded$controllers)
  expect_equal(next_state$game$hand_number,2)
  expect_length(next_state$completed,1)
  expect_null(next_state$completed[[1]]$players)
  expect_error(table_next(next_state,version=1),"turn changed")
  expect_error(table_next(initial),"cannot continue")
})

test_that("eliminated poker seats are not shown with fictional hidden cards", {
  done <- holdem_action(new_holdem(c(100,100,100),seed=1),"fold")
  done <- holdem_action(done,"fold")
  done$players[[2]]$points <- 0
  done$players[[1]]$points <- 100
  done$players[[3]]$points <- 200
  next_hand <- holdem_next_hand(done,seed=2)
  view <- game_table_view(next_hand)
  expect_length(view$seats[[2]]$hands[[1]]$cards,0)
  expect_length(view$seats[[3]]$hands[[1]]$cards,2)
  expect_equal(view$viewer,1)
  expect_true(all(vapply(view$seats[[3]]$hands[[1]]$cards,`[[`,logical(1),"back")))
  terminal <- done
  terminal$players[[1]]$points <- 300
  terminal$players[[3]]$points <- 0
  expect_false(game_table_view(terminal)$can_next)
  expect_match(paste(game_table_view(terminal)$result,collapse=" "),"Table finished")
})

test_that("Shiny next-hand clicks preserve stacks and reject old turn versions", {
  skip_if_not_installed("shiny")
  initial <- table_session(new_holdem(seed=1))
  shiny::testServer(table_app_server(initial,"en"),{
    session$flushReact()
    session$setInputs(decision=list(action="fold",version=0))
    expect_true(view()$can_next)
    session$setInputs(table_continue=list(version=1))
    expect_equal(state()$game$hand_number,2)
    expect_equal(state()$game$initial_stacks,c(99,101))
    expect_length(selected(),0)
    session$setInputs(table_continue=list(version=1))
    expect_equal(state()$game$hand_number,2)
    expect_match(state()$error,"turn changed")
  })
})
