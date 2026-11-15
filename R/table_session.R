# Server-side session transitions are independent of Shiny and never serialized.
table_default_controllers <- function(game) {
  maker <- if(inherits(game,"holdem_hand")) function() poker_bot(equity_samples=64) else
    if(inherits(game,"mus_game")) function() mus_bot(equity_samples=16) else
      if(inherits(game,"blackjack_game")) blackjack_bot else if(inherits(game,"scopa_game")) scopa_bot else
        if(inherits(game,"liar_game")) liar_bot else stop("Unsupported table game")
  lapply(seq_along(game$players),function(i) maker())
}

table_session <- function(game,seat=1L,controllers=NULL,seed=42L,max_bot_actions=500L) {
  game_table_view(game,seat)
  check_number(seed,"seed",0,.Machine$integer.max,TRUE)
  check_number(max_bot_actions,"max_bot_actions",1,100000L,TRUE)
  if(is.null(controllers)) controllers <- table_default_controllers(game)
  if(!is.list(controllers) || length(controllers)!=length(game$players)) stop("Provide one controller entry per seat")
  for(i in setdiff(seq_along(controllers),seat))
    if(!is_game_controller(controllers[[i]]) || inherits(controllers[[i]],"console_player")) stop("Other seats need non-console controllers")
  controllers[seat] <- list(NULL)
  list(game=game,seat=seat,controllers=controllers,seed=seed,version=0L,
    actions=0L,bot_actions=0L,max_bot_actions=max_bot_actions,paused=FALSE,error=NULL,
    completed=list())
}

table_step_seed <- function(state) (state$seed+as.double(state$actions)) %% .Machine$integer.max

table_apply <- function(state,decision,version=state$version) {
  if(!identical(as.integer(version),as.integer(state$version))) stop("The turn changed; choose again")
  if(state$paused) stop("Resume play first")
  if(!identical(as.integer(state$game$actor),as.integer(state$seat)) || game_hand_complete(state$game)) stop("Wait for your turn")
  updated <- with_poker_seed(table_step_seed(state),function() game_action(state$game,decision))
  state$game <- updated;state$actions <- state$actions+1L;state$version <- state$version+1L;state$error <- NULL
  state
}

table_bot_step <- function(state) {
  if(state$paused || game_hand_complete(state$game) || identical(as.integer(state$game$actor),as.integer(state$seat))) return(state)
  if(state$bot_actions>=state$max_bot_actions) {
    state$paused <- TRUE;state$error <- "Automatic play limit reached; resume to continue";state$version <- state$version+1L
    return(state)
  }
  i <- state$game$actor
  update <- with_poker_seed(table_step_seed(state),function() {
    choice <- choose_action(state$controllers[[i]],game_observation(state$game))
    if(!is.list(choice) || !is_game_controller(choice$controller) || inherits(choice$controller,"console_player")) stop("Invalid bot controller result")
    list(game=game_action(state$game,choice$decision),controller=choice$controller)
  })
  state$game <- update$game;state$controllers[i] <- list(update$controller)
  state$actions <- state$actions+1L;state$bot_actions <- state$bot_actions+1L
  state$version <- state$version+1L;state$error <- NULL
  state
}

table_continuation <- function(game,seat=NULL) {
  available <- game_hand_complete(game) && !game_complete(game) &&
    (inherits(game,"mus_game") || inherits(game,"scopa_game"))
  message <- NULL
  if(inherits(game,"holdem_hand") && game_hand_complete(game)) {
    available <- sum(holdem_stacks(game)>0)>=2L
    if(!available) message <- "Table finished: fewer than two players have chips."
    if(!is.null(seat) && game$players[[seat]]$points<=0) {
      available <- FALSE
      message <- "Your bankroll is empty. Start a new table to play again."
    }
  }
  label <- "Next hand"
  if(inherits(game,"blackjack_game") && game_hand_complete(game)) {
    label <- "Next round"
    available <- if(is.null(seat)) any(vapply(game$players,`[[`,0,"points")>=blackjack_round_wagers(game)) else
      game$players[[seat]]$points>0
    if(!available) message <- "Your bankroll is empty. Start a new table to play again."
  }
  list(available=available,label=label,message=message)
}

table_next <- function(state,version=state$version,wager=NULL) {
  if(!identical(as.integer(version),as.integer(state$version))) stop("The turn changed; choose again")
  if(!table_continuation(state$game,state$seat)$available) stop("This table cannot continue yet")
  result <- game_result(state$game)
  state$game <- with_poker_seed(table_step_seed(state),function() {
    if(inherits(state$game,"blackjack_game")) {
      wagers <- blackjack_round_wagers(state$game)
      if(is.null(wager)) wager <- min(wagers[state$seat],state$game$players[[state$seat]]$points)
      check_number(wager,"wager",0,state$game$players[[state$seat]]$points)
      if(wager<=0) stop("Choose a positive wager within your bankroll")
      wagers[state$seat] <- wager
      blackjack_next_round(state$game,wagers)
    } else game_next_hand(state$game)
  })
  state$completed <- utils::tail(c(state$completed,list(result)),20L)
  state$version <- state$version+1L;state$actions <- state$actions+1L;state$bot_actions <- 0L
  state$error <- NULL;state$paused <- FALSE
  state
}
