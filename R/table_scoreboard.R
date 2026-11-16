# Public, bounded completion history; the current result is included before Next.
table_session_results <- function(state) {
  results <- state$completed
  if(game_hand_complete(state$game)) results <- c(results,list(game_result(state$game)))
  utils::tail(results,20L)
}

table_scoreboard <- function(state,language="en") {
  results <- table_session_results(state)
  names <- vapply(state$game$players,`[[`,"","name")
  stacks <- table_bankrolls(state$game)
  wins <- state$wins
  winners <- table_winners(state$game);wins[winners] <- wins[winners]+1L
  financial <- inherits(state$game,"holdem_hand") || inherits(state$game,"blackjack_game")
  rows <- lapply(seq_along(names),function(i) shiny::tags$tr(shiny::tags$td(names[i]),shiny::tags$td(wins[i]),
    if(financial) shiny::tags$td(stacks[i]),
    if(financial) shiny::tags$td(stacks[i]-state$starting_stacks[i])))
  shiny::tags$details(class="rc-session",shiny::tags$summary(ui("Session history",language)),
    shiny::tags$p(ui("Completed hands or deals: %s",language,state$completed_count+as.integer(game_hand_complete(state$game)))),
    shiny::tags$table(class="rc-scoreboard",shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(ui("Players",language)),shiny::tags$th(ui("Wins",language)),
      if(financial) shiny::tags$th(ui("Bankroll",language)),if(financial) shiny::tags$th(ui("Change since start",language)))),shiny::tags$tbody(rows)),
    if(!financial) shiny::tags$p(ui("Current scores: %s",language,
      if(is.null(state$game$scores)) "\u2014" else paste(state$game$scores,collapse=" : "))),
    shiny::tags$p(ui("Latest 20 completed results",language)),
    shiny::tags$ol(lapply(results,function(result) shiny::tags$li(
      paste(game_summary_text(result,language),collapse=" | ")))))
}

table_bankrolls <- function(game) {
  chips <- vapply(game$players,`[[`,0,"points")
  if(!game_hand_complete(game)) {
    if(inherits(game,"holdem_hand")) chips <- chips+game$contributions
    if(inherits(game,"blackjack_game")) chips <- chips+vapply(game$hands,function(hands)
      sum(vapply(hands,`[[`,0,"bet")),0)
  }
  chips
}

table_winners <- function(game) {
  if(!game_hand_complete(game)) return(integer())
  if(inherits(game,"holdem_hand") || inherits(game,"blackjack_game"))
    return(which(table_bankrolls(game)>game$initial_stacks))
  if(!game_complete(game)) return(integer())
  if(inherits(game,"liar_game")) return(game$winner)
  teams <- if(inherits(game,"mus_game")) mus_team(seq_along(game$players)) else game$teams
  which(teams %in% game$winner)
}
