#' Project a game onto a shared graphical table
#'
#' A renderer-neutral view for one fixed viewer seat. Backed cards contain no
#' face data. Revealed opponents are included only at legitimate showdowns.
#' The returned view never contains a shoe, face-down pile, full game object,
#' hidden truth flags or controller state. Names/display strings are plain text.
#' @param game A supported game object.
#' @param seat Viewer seat, independent of the current actor.
#' @param language Display language en, es, de or it.
#' @return A list of seats, card zones, public status, legal controls and results.
#' @export
game_table_view <- function(game,seat=1L,language="en") {
  language <- console_language(language)
  check_number(seat,"seat",1,length(game$players),TRUE)
  o <- game_observation(game,seat)
  type <- o$game_type
  if(!type %in% c("holdem","mus","blackjack","scopa","liar")) stop("Unsupported table game")
  done <- game_hand_complete(game)
  your_turn <- !done && identical(as.integer(o$actor),as.integer(seat))
  legal <- if(your_turn) game_legal_actions(game) else list(actions=character())
  face <- function(card) {
    if(is.null(card)) return(list(back=TRUE))
    label <- translate_card(card,language)$display
    list(back=FALSE,value=card$value,suit=card$suit,symbol=card$symbol,label=label)
  }
  faces <- function(cards) lapply(cards,face)
  backs <- function(count) rep(list(list(back=TRUE)),count)
  zone <- function(id,label,cards,selectable=FALSE) list(id=id,label=label,cards=cards,selectable=selectable)
  seats <- lapply(seq_along(game$players),function(i) list(seat=i,name=o$player_names[i],
    active=!done && identical(as.integer(o$actor),as.integer(i)),viewer=i==seat,
    details=character(),hands=list()))
  center <- list();own <- list();status <- character()
  selectable <- your_turn && any(c("discard","play","claim") %in% legal$actions)
  if(type=="blackjack") {
    for(i in seq_along(seats)) {
      for(j in seq_along(o$hand_counts[[i]])) {
        cards <- if(i==seat) faces(o$hands[[j]]$cards) else if(done) faces(game$hands[[i]][[j]]$cards) else backs(o$hand_counts[[i]][j])
        label <- if(i==seat) ui("Hand %s | Total %s | Wager %s",language,j,
          blackjack_total(o$hands[[j]]$cards)$total,o$hands[[j]]$bet) else paste(j)
        hand <- zone(paste0("hand_",j),label,cards)
        hand$active <- your_turn && j==o$active_hand
        if(i==seat) own[[j]] <- hand else seats[[i]]$hands[[j]] <- hand
      }
      seats[[i]]$details <- ui("Stacks: %s",language,o$stacks[i])
    }
    center <- list(zone("dealer",ui("Dealer",language),faces(o$dealer)))
  } else {
    counts <- if(type=="holdem") rep(2L,length(seats)) else if(type=="mus") rep(4L,length(seats)) else o$hand_counts
    revealed <- if(type %in% c("holdem","mus")) console_revealed_hands(game) else integer()
    for(i in seq_along(seats)) {
      cards <- if(i==seat) faces(o$hole) else if(i %in% revealed) faces(game$players[[i]]$hand) else backs(counts[i])
      if(i==seat) own <- list(zone("hand",ui("Your cards",language),cards,selectable)) else
        seats[[i]]$hands <- list(zone(paste0("seat_",i),"",cards))
      if(type=="holdem") seats[[i]]$details <- c(ui("Stacks: %s",language,o$stacks[i]),if(o$folded[i]) ui("folded",language))
      if(type %in% c("mus","scopa")) seats[[i]]$details <- paste0(ui("Side %s",language,o$teams[i])," | ",o$scores[o$teams[i]])
      if(type=="liar") seats[[i]]$details <- as.character(counts[i])
    }
    if(type=="holdem") {
      center <- list(zone("board",ui("Board",language),faces(o$board)))
      status <- ui("Pot %s  |  To call %s  |  Your stack %s",language,o$pot,
        if(done) 0 else if(your_turn) legal$to_call else max(0,game$current_bet-o$round_bets[seat]),o$stacks[seat])
    }
    if(type=="mus") status <- c(ui(toupper(o$phase),language),ui("Stake %s",language,if(o$ordago) "ORDAGO" else o$wager))
    if(type=="scopa") {
      center <- list(zone("table",ui("Table cards",language),faces(o$table_cards)))
      status <- ui("Deal %s | Stock %s | Scores %s",language,o$hand_number,o$stock_count,paste(o$scores,collapse=" : "))
    }
    if(type=="liar") {
      center <- list(zone("pile",ui("Pile %s | Declared rank %s",language,o$pile_count,
        if(is.null(o$rank)) ui("choose a rank",language) else o$rank),backs(as.integer(o$pile_count>0))))
      if(!is.null(o$claim)) status <- ui("Claim: %s | Rank: %s | Cards: %s",language,
        o$player_names[o$claim$player],o$claim$rank,o$claim$count)
      challenges <- Filter(function(event) event$action=="challenge",o$history)
      if(length(challenges)) {
        reveal <- utils::tail(challenges,1)[[1]]
        center[[2]] <- zone("reveal",ui("Latest batch revealed",language),faces(reveal$cards))
        status <- c(status,ui(if(reveal$caught) "Caught bluff" else "Truthful claim",language),
          ui("Pickup: %s | Cards: %s",language,o$player_names[reveal$picker],reveal$pickup_count))
      }
    }
  }
  result <- if(done) table_result_text(game,language) else character()
  list(game_type=type,phase=if(is.null(o$phase)) o$street else o$phase,
    viewer=seat,actor=o$actor,seats=seats,center=center,own=own,status=status,
    your_turn=your_turn,hand_complete=done,complete=game_complete(game),
    can_next=done && !game_complete(game) && type %in% c("mus","scopa"),
    selection_limit=if(!selectable) 0L else if(type=="scopa") 1L else if(type=="liar") legal$max_cards else 4L,
    legal=legal,result=result,language=language)
}

table_result_text <- function(game,language) {
  if(inherits(game,"liar_game")) return(ui("Winner: %s",language,game$players[[game$winner]]$name))
  if(inherits(game,"blackjack_game")) return(vapply(game$results,function(r)
    ui("Hand %s: %s | Returned %s",language,paste0(game$players[[r$player]]$name," / ",r$hand),ui(r$outcome,language),r$returned),""))
  if(inherits(game,"scopa_game")) return(console_scopa_end(game,console_player(language=language)))
  if(inherits(game,"mus_game")) return(c(ui("Scores %s : %s",language,game$scores[1],game$scores[2]),
    if(game$complete) ui("Winner: %s",language,paste(vapply(game$players[mus_team(1:4)==game$winner],`[[`,"","name"),collapse=" & "))))
  vapply(Filter(function(pot) !pot$uncalled,game$pots),function(pot) {
    names <- vapply(game$players[pot$winners],`[[`,"","name")
    key <- if(length(names)>1L) "%s split %s %s%s." else if(tolower(names)=="you") "%s win %s %s%s." else "%s wins %s %s%s."
    ui(key,language,paste(names,collapse=" & "),ui("pot",language),pot$amount,"")
  },"")
}

#' Translate a table interaction into a canonical engine decision
#' @param view A current game_table_view().
#' @param action One of the current canonical legal actions.
#' @param selected Selected own-hand positions, never table-card positions.
#' @param amount Total street contribution in Poker, or total Mus stake.
#' @param rank Declared Mentiroso rank.
#' @param capture Scopa capture-option index for the selected hand card.
#' @return A decision for game_action(). The engine remains the authority on
#' legality; this function does not apply the decision or alter the view.
#' @export
table_decision <- function(view,action,selected=integer(),amount=NULL,rank=NULL,capture=NULL) {
  if(!isTRUE(view$your_turn) || length(action)!=1L || is.na(action) || !action %in% view$legal$actions)
    stop("Choose one of the listed legal actions")
  if(!is.numeric(selected) || anyNA(selected)) stop("Select valid hand positions")
  if(action=="play") {
    if(length(selected)!=1L) stop("Choose a listed hand position")
    validate_positions(selected,length(view$legal$captures))
    options <- view$legal$captures[[selected]]
    if(is.null(capture) || !length(capture) || is.na(capture) || capture==0) {
      if(length(options)>1L) stop("Choose a capture option")
      capture <- 1L
    }
    return(list(action="play",cards=selected,amount=capture))
  }
  if(action=="claim") return(list(action="claim",cards=selected,amount=rank))
  if(action=="discard") return(list(action="discard",cards=selected))
  if(action %in% c("raise","bet")) return(list(action=action,amount=amount))
  list(action=action)
}
