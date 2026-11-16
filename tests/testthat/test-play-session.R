test_that("console sessions retain chips and stop on a declined continuation", {
  inputs <- c("fold","yes","no")
  read <- function(prompt) {x<-inputs[1];inputs<<-inputs[-1];x}
  bot <- policy_player(function(o,state) list(decision=list(action="fold"),state=state))
  result <- play_session(new_holdem(seed=1),list(console_player(read=read,write=function(x) NULL),bot),seed=3)
  expect_equal(result$game$hand_number,2)
  expect_equal(sum(holdem_stacks(result$game)),200)
  expect_equal(result$session_status,"stopped")
  expect_length(result$session_results,2)
  expect_null(result$session_results[[1]]$players)
})

test_that("console confirmation limits and EOF preserve the completed game", {
  for(answers in list(c("fold",NA_character_),c("fold","?","?"))) {
    read <- local({pending<-answers;function(prompt){x<-pending[1];pending<<-pending[-1];x}})
    controllers <- list(console_player(read=read,write=function(x) NULL),function(o) list(action="fold"))
    set.seed(12);before <- .Random.seed
    result <- play_session(new_holdem(seed=1),controllers,max_invalid=2,seed=9)
    expect_identical(.Random.seed,before)
    expect_true(game_hand_complete(result$game))
    expect_equal(result$game$hand_number,1)
    expect_equal(result$session_status,if(is.na(answers[2])) "stopped" else "input_limit")
  }
})
