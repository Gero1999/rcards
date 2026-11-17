#' Play repeated hands, rounds, or matches in the console
#'
#' Shows each result before asking the first console controller whether to
#' continue. Retains controllers, settled bankrolls, rules and seating. Mus and
#' Scopa ask explicitly before resetting scores for a new match. Blackjack uses
#' a fresh shoe and displays the next base wager before confirmation. A reduced
#' wager is offered if the remaining bankroll cannot cover the previous wager.
#' @param game A supported playable game.
#' @param controllers One controller per seat, including a console_player().
#' @param max_hands Maximum hands/rounds in this call, including the current one.
#' @param max_actions Maximum actions per hand, forwarded to play().
#' @param max_invalid Maximum invalid answers before returning input_limit.
#' @param seed Optional session seed preserving the caller's RNG.
#' @return The last game_play result, with session_results (public summaries)
#' and session_status: stopped, table_complete, hand_limit, or the play status.
#' Resume by supplying its game and controllers to another call.
#' @export
play_session <- function(game,controllers,max_hands=100L,max_actions=10000L,
                         max_invalid=20L,seed=NULL) {
  check_number(max_hands,"max_hands",1,100000L,TRUE)
  check_number(max_invalid,"max_invalid",1,100000L,TRUE)
  humans <- which(vapply(controllers,function(x) inherits(x,"console_player"),logical(1)))
  if(!length(humans)) stop("A console session needs a console player")
  seat <- humans[1L]
  with_poker_seed(seed,function() {
    history <- list()
    for(round in seq_len(max_hands)) {
      result <- play(game,controllers,until="hand",max_actions=max_actions,max_invalid=max_invalid)
      game <- result$game;controllers <- result$controllers
      finish <- function(status) {
        result$session_results <- history;result$session_status <- status;result
      }
      if(!game_hand_complete(game)) return(finish(result$status))
      history <- c(history,list(game_result(game)))
      continuation <- table_continuation(game,seat)
      if(!continuation$available) return(finish("table_complete"))
      if(round==max_hands) return(finish("hand_limit"))
      human <- controllers[[seat]]
      if(inherits(game,"blackjack_game")) human$write(ui("Next wager: %s",human$language,
        min(blackjack_round_wagers(game)[seat],game$players[[seat]]$points)))
      accepted <- FALSE
      for(attempt in seq_len(max_invalid)) {
        answer <- tryCatch(console_read(human,paste0(ui(continuation$label,human$language),
          ui("? Continue [y/n]: ",human$language))),interrupt=function(e) NA_character_)
        if(!length(answer) || length(answer)!=1L || is.na(answer)) return(finish("stopped"))
        answer <- tolower(trimws(answer))
        if(answer %in% c("","n","no","\u5426","nein","q","quit","exit")) return(finish("stopped"))
        if(answer %in% c("y","yes","\u662f","\u7ee7\u7eed","sim","s","si","s\u00ed","j","ja","s\u00ec")) {accepted<-TRUE;break}
        human$write(ui("Answer yes or no",human$language))
      }
      if(!accepted) return(finish("input_limit"))
      state <- table_session(game,seat,controllers,seed=sample.int(.Machine$integer.max,1L)-1L)
      game <- table_next(state)$game
    }
  })
}
