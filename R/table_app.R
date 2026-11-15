#' Play card games at a local graphical table
#'
#' Optional Shiny interface with one fixed human seat and bots in other seats.
#' Game rules and legality stay in the existing engines. Each browser session
#' owns its own game/controller state; no hidden engine state is sent to clients.
#' This is local single-human play, not authenticated network multiplayer.
#' @param game Optional configured Holdem, Mus, Blackjack, Scopa or Liar game.
#' NULL starts a two-player Holdem table. The UI can start fresh default games.
#' @param seat Human viewer seat in the supplied game, default one.
#' @param controllers Optional controller list; the human entry is ignored.
#' Other seats require non-console controllers. NULL selects baseline bots.
#' @param language Initial language: en, es, de or it.
#' @param seed Session seed preserving caller RNG; default 42.
#' @param max_bot_actions Automatic actions before pausing for confirmation.
#' @param launch TRUE runs locally on 127.0.0.1 and opens a browser. FALSE returns
#' a shiny.appobj for testing or custom local launching. Shiny is optional.
#' @return A Shiny application when launch=FALSE; otherwise the local runApp()
#' result invisibly. Install shiny separately to use this interface.
#' @export
play_app <- function(game=NULL,seat=1L,controllers=NULL,language="en",seed=42L,
                     max_bot_actions=500L,launch=TRUE) {
  if(!requireNamespace("shiny",quietly=TRUE)) stop("Install the optional shiny package to use play_app()")
  language <- console_language(language)
  check_number(seed,"seed",0,.Machine$integer.max,TRUE)
  if(!is.logical(launch) || length(launch)!=1L || is.na(launch)) stop("launch must be TRUE or FALSE")
  if(is.null(game)) game <- table_new_game("holdem",2L,seed)
  initial <- table_session(game,seat,controllers,seed,max_bot_actions)
  app <- shiny::shinyApp(table_app_ui(initial,language),table_app_server(initial,language))
  if(!launch) return(app)
  invisible(shiny::runApp(app,host="127.0.0.1",launch.browser=TRUE))
}

table_new_game <- function(type,n,seed) {
  n <- as.integer(n)
  choices <- table_player_choices(type)
  if(length(n)!=1L || is.na(n) || !n %in% choices) stop("Choose a supported player count")
  names <- c("You",if(n>1L) paste0("Bot ",seq_len(n-1L)))
  players <- lapply(names,function(name) player(name,points=100))
  switch(type,holdem=holdem_hand(players,seed=seed),mus=mus_game(players,seed=seed),
    blackjack=blackjack_game(players,seed=seed),scopa=scopa_game(players,seed=seed),
    liar=liar_game(players,seed=seed),stop("Unsupported table game"))
}

table_player_choices <- function(type) switch(type,holdem=2:10,mus=4L,blackjack=1L,scopa=2:4,liar=2:8,integer())
table_game_names <- function() c("Texas Hold'em"="holdem","Mus"="mus","Blackjack"="blackjack","Scopa"="scopa","Mentiroso"="liar")

table_app_ui <- function(initial,language) {
  type <- game_table_view(initial$game,initial$seat)$game_type
  assets <- system.file("app",package="rcards")
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$title("rcards | Card table"),
      shiny::includeCSS(file.path(assets,"table.css")),shiny::includeScript(file.path(assets,"table.js"))),
    shiny::tags$div(class="rc-app",
      shiny::tags$header(class="rc-header",shiny::tags$div(class="rc-brand","rcards",shiny::tags$span("CARD TABLE")),
        shiny::uiOutput("heading")),
      shiny::tags$section(class="rc-setup",`aria-label`="Table settings",
        shiny::selectInput("game_kind",ui("Game",language),table_game_names(),selected=type,selectize=FALSE),
        shiny::selectInput("players",ui("Players",language),table_player_choices(type),selected=length(initial$game$players),selectize=FALSE),
        shiny::selectInput("language",ui("Language",language),c(English="en","Espa\u00f1ol"="es",Deutsch="de",Italiano="it"),selected=language,selectize=FALSE),
        shiny::uiOutput("new_control")),
      shiny::tags$main(shiny::uiOutput("table"),
        shiny::tags$section(class="rc-feedback",`aria-live`="polite",shiny::uiOutput("feedback")),
        shiny::tags$section(class="rc-controls",shiny::uiOutput("controls"))),
      shiny::tags$footer(class="rc-footer",shiny::uiOutput("footer"))))
}

table_app_server <- function(initial,initial_language) {
  function(input,output,session) {
    state <- shiny::reactiveVal(initial)
    selected <- shiny::reactiveVal(integer())
    lang <- shiny::reactive(console_language(if(is.null(input$language)) initial_language else input$language))
    view <- shiny::reactive(game_table_view(state()$game,state()$seat,lang()))
    report_error <- function(error) {
      current <- shiny::isolate(state());current$error <- conditionMessage(error);state(current)
    }
    shiny::observeEvent(input$game_kind,{
      choices <- table_player_choices(input$game_kind)
      n <- suppressWarnings(as.integer(input$players))
      if(length(n)!=1L || is.na(n) || !n %in% choices) n <- if(input$game_kind=="scopa") 4L else if(input$game_kind=="liar") 3L else choices[1]
      shiny::updateSelectInput(session,"players",choices=choices,selected=n)
    },ignoreNULL=TRUE)
    shiny::observe({
      language <- lang()
      shiny::updateSelectInput(session,"game_kind",label=ui("Game",language))
      shiny::updateSelectInput(session,"players",label=ui("Players",language))
      shiny::updateSelectInput(session,"language",label=ui("Language",language))
    })
    shiny::observeEvent(input$new_table,{
      tryCatch({
        old <- state();seed <- (old$seed+old$version+1) %% .Machine$integer.max
        game <- table_new_game(input$game_kind,input$players,seed)
        current <- table_session(game,1L,seed=seed,max_bot_actions=old$max_bot_actions)
        current$version <- old$version+1L
        state(current);selected(integer())
      },error=report_error)
    },ignoreInit=TRUE)
    shiny::observeEvent(input$pause_table,{
      current <- state();current$paused <- !current$paused;current$version <- current$version+1L
      if(!current$paused) {current$bot_actions<-0L;current$error<-NULL}
      state(current)
    },ignoreInit=TRUE)
    shiny::observeEvent(input$table_continue,{
      tryCatch({
        event <- input$table_continue
        if(!is.list(event) || length(event$version)!=1L || is.na(event$version)) stop("The turn changed; choose again")
        state(table_next(state(),event$version,event$wager));selected(integer())
      },error=report_error)
    },ignoreInit=TRUE)
    shiny::observeEvent(input$card_pick,{
      event <- input$card_pick;v <- view();current <- state()
      if(!is.list(event) || !identical(as.integer(event$version),as.integer(current$version)) ||
         !identical(event$zone,"hand") || !v$your_turn || current$paused || v$selection_limit==0L) return()
      index <- suppressWarnings(as.integer(event$index))
      if(length(index)!=1L || is.na(index) || !length(v$own) || index<1L || index>length(v$own[[1]]$cards)) return()
      choices <- selected()
      if(index %in% choices) choices <- setdiff(choices,index) else if(v$selection_limit==1L) choices <- index else
        if(length(choices)<v$selection_limit) choices <- sort(c(choices,index))
      selected(choices)
    },ignoreInit=TRUE)
    shiny::observeEvent(input$decision,{
      tryCatch({
        event <- input$decision;current <- state()
        if(!is.list(event) || length(event$version)!=1L || is.na(event$version)) stop("The turn changed; choose again")
        v <- view()
        amount <- if("amount" %in% names(event)) event$amount else input$amount
        rank <- if("rank" %in% names(event)) event$rank else input$rank
        capture <- if("capture" %in% names(event)) event$capture else input$capture
        decision <- table_decision(v,event$action,selected(),amount,rank,table_capture_index(capture,selected()))
        state(table_apply(current,decision,event$version));selected(integer())
      },error=report_error)
    },ignoreInit=TRUE)
    # The timer, not a change to state(), schedules bot turns. One decision per
    # tick keeps the browser responsive and pauses before the human must act.
    shiny::observe({
      shiny::invalidateLater(350,session)
      current <- shiny::isolate(state())
      if(!current$paused && !game_hand_complete(current$game) &&
         !identical(as.integer(current$game$actor),as.integer(current$seat))) {
        tryCatch({
          next_state <- table_bot_step(current)
          state(next_state);selected(integer())
        },error=function(error) {
          current$paused <- TRUE;current$error <- "Bot could not continue; start a new table or change controllers"
          current$version <- current$version+1L;state(current)
        })
      }
    })
    output$new_control <- shiny::renderUI(shiny::actionButton("new_table",ui("New table",lang()),class="rc-primary"))
    output$heading <- shiny::renderUI({
      v <- view();label <- if(v$hand_complete) ui("Hand complete",lang()) else
        if(state()$paused) ui("Paused",lang()) else if(v$your_turn) ui("Your turn",lang()) else ui("Bots are playing",lang())
      shiny::tags$span(class=paste("rc-turn",if(v$your_turn) "human"),label)
    })
    output$table <- shiny::renderUI(table_render_view(view(),selected(),input$capture,state()$version))
    output$feedback <- shiny::renderUI({
      current <- state();v <- view()
      shiny::tagList(
        if(!is.null(current$error)) shiny::tags$p(class="rc-error",role="alert",console_error(current$error,lang())),
        lapply(v$result,function(line) shiny::tags$p(class="rc-result",line)))
    })
    output$controls <- shiny::renderUI({
      # Selection/capture changes must not reconstruct numeric/rank inputs and
      # erase what the player is typing. Only engine turn changes rebuild them.
      current <- state();language <- lang();v <- game_table_view(current$game,current$seat,language)
      table_render_controls(v,current$version,current$paused)
    })
    output$capture_controls <- shiny::renderUI({
      v <- view();indices <- selected()
      if(v$game_type!="scopa" || !v$your_turn || length(indices)!=1L) return(NULL)
      options <- v$legal$captures[[indices]]
      labels <- vapply(seq_along(options),function(j) {
        take <- options[[j]]
        paste0(j,": ",if(length(take)) ui("Take table positions %s",lang(),paste(take,collapse=" + ")) else ui("leave on table",lang()))
      },"")
      shiny::radioButtons("capture",ui("Capture choice",lang()),choices=stats::setNames(paste0(indices,":",seq_along(options)),labels),
        selected=if(length(options)==1L) paste0(indices,":1") else character())
    })
    output$footer <- shiny::renderUI(shiny::tagList(
      shiny::tags$span(ui("Local play against bots. Your seat: %s",lang(),view()$seats[[view()$viewer]]$name)),
      shiny::actionButton("pause_table",ui(if(state()$paused) "Resume" else "Pause",lang())),
      if(!is.null(view()$next_wager)) shiny::numericInput("next_wager",ui("Next round wager",lang()),
        value=view()$next_wager$value,min=0,max=view()$next_wager$maximum,step=1),
      if(view()$can_next) shiny::tags$button(type="button",id="next_hand",class="btn btn-default",
        `data-rc-continue`="next",`data-rc-version`=state()$version,view()$continue_label)))
  }
}
