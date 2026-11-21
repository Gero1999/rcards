# Suit artwork is shared by card identity across every table that uses the deck.
table_suit_mark <- function(card) {
  icon <- switch(card$suit,
    Oros =, Denari = "coin",
    Copas =, Coppe = "cup",
    Espadas =, Spade = "sword",
    Bastos =, Bastoni = "baton",
    Joker = "joker"
  )
  if (is.null(icon)) return(shiny::tags$span(class = "rc-suit-mark", card$symbol))
  shiny::tags$span(class = paste0("rc-suit-mark rc-suit-", icon))
}

table_card_tag <- function(card,index,zone,version,selectable=FALSE,selected=FALSE,highlight=FALSE,language="en",indexed=selectable) {
  classes <- paste("rc-card",if(card$back) "rc-back" else if(card$suit %in% c("Hearts","Diamonds")) "rc-red" else "rc-black",
    if(selectable) "rc-selectable",if(selected) "rc-selected",if(highlight) "rc-highlight")
  if (card$back) {
    label <- ui("Covered card", language)
    content <- shiny::tags$span(class = "rc-back-mark", `aria-hidden` = "true", "rc")
  } else {
    description <- card$label
    suffix <- paste0(" ", card$symbol)
    if (endsWith(description, suffix)) {
      description <- substr(description, 1L, nchar(description) - nchar(suffix))
    }
    label <- paste0(index, ": ", description)
    rank <- if (card$value == "Joker" && card$suit == "Joker") "JK" else card$value
    suit <- table_suit_mark(card)
    content <- shiny::tagList(
      shiny::tags$span(class = "rc-rank", `aria-hidden` = "true", rank, suit),
      shiny::tags$span(class = "rc-suit", `aria-hidden` = "true", suit),
      shiny::tags$span(class = "rc-rank rc-bottom", `aria-hidden` = "true", rank, suit),
      if (indexed) shiny::tags$span(class = "rc-index", `aria-hidden` = "true", index)
    )
  }
  shiny::tags$button(type="button",class=classes,title=label,`aria-label`=label,
    `aria-pressed`=if(selectable) tolower(as.character(selected)) else NULL,
    `data-rc-card`=if(selectable) as.character(index) else NULL,`data-rc-zone`=zone,
    `data-rc-version`=version,disabled=if(!selectable) "disabled" else NULL,content)
}

table_render_zone <- function(zone,version,selected=integer(),highlight=integer(),language="en") {
  shiny::tags$section(class=paste("rc-zone",if(isTRUE(zone$active)) "rc-active-hand"),`data-zone`=zone$id,
    if(nzchar(zone$label)) shiny::tags$h3(zone$label),
    shiny::tags$div(class="rc-cards",if(!length(zone$cards)) shiny::tags$span(class="rc-empty",ui("(none)",language)) else
      lapply(seq_along(zone$cards),function(i) table_card_tag(zone$cards[[i]],i,zone$id,version,
        isTRUE(zone$selectable),i %in% selected,i %in% highlight,language,
        indexed=isTRUE(zone$selectable) || zone$id=="table"))))
}

table_render_center_cards <- function(view,version,selected,capture) {
  language <- view$language;highlight <- integer()
  if(view$game_type=="scopa" && view$your_turn && length(selected)==1L) {
    index <- table_capture_index(capture,selected);options<-view$legal$captures[[selected]]
    if(!is.null(index) && index>=1 && index<=length(options)) highlight <- options[[index]]
  }
  zones <- view$center
  if(view$game_type=="holdem") zones <- Filter(function(zone) length(zone$cards)>0L,zones)
  lapply(zones,table_render_zone,version=version,highlight=highlight,language=language)
}

table_stock_tag <- function(zone,label,language,version) {
  shiny::tags$div(class="rc-stock",`data-zone`=zone,
    shiny::tags$h3(ui(label,language)),
    shiny::tags$div(class="rc-stock-back",`aria-hidden`="true",
      table_card_tag(list(back=TRUE),1L,zone,version,language=language)))
}

table_render_view <- function(view,version,hand_controls=NULL,pile_controls=NULL,board_controls=NULL) {
  language <- view$language
  board_cards <- shiny::uiOutput("table_center_cards")
  own_cards <- shiny::uiOutput("table_own_cards",class="rc-own-zones")
  if(view$game_type %in% c("holdem","blackjack")) {
    stock <- if(view$game_type=="holdem") table_stock_tag("stock","Deck",language,version) else
      table_stock_tag("shoe","Shoe",language,version)
    board_cards <- shiny::tags$div(class="rc-board-layout",
      shiny::tags$div(class="rc-board-cards",stock,board_cards),
      board_controls)
  }
  if(view$game_type=="holdem") {
    own_cards <- shiny::tags$div(class="rc-hand-layout",own_cards,hand_controls)
    hand_controls <- NULL
  }
  opponents <- view$seats[!vapply(view$seats,`[[`,logical(1),"viewer")]
  own_name <- view$seats[[view$viewer]]$name
  shiny::tags$section(class="rc-table",`aria-label`=ui("Card table",language),
    shiny::tags$div(class="rc-opponents",lapply(opponents,function(player) {
      shiny::tags$section(class=paste("rc-seat",if(player$active) "rc-active-seat"),`data-rc-seat`=player$seat,
        shiny::tags$h2(player$name,if(player$seat %in% view$dealer) table_dealer_tag(language)),
        shiny::tags$p(paste(player$details,collapse=" | ")),
        lapply(player$hands,table_render_zone,version=version,language=language))
    })),
    shiny::tags$div(class="rc-center",
      shiny::tags$h1(class="rc-game-title",names(table_game_names())[match(view$game_type,table_game_names())],
        if(!is.null(view$phase)) shiny::tags$small(ui(toupper(view$phase),language))),lapply(view$status,function(line) shiny::tags$p(class="rc-status",line)),
      if(!is.null(view$pot)) table_pot_tag(view$pot,language),
      board_cards,
      pile_controls,
      if(length(view$table_details)) shiny::tags$details(class="rc-table-details",
        shiny::tags$summary(ui("Table details",language)),shiny::tags$p(view$table_details)),
      if(length(view$history)) shiny::tags$details(class="rc-history",shiny::tags$summary(ui("Recent actions",language)),
        shiny::tags$ol(lapply(view$history,shiny::tags$li)))),
    shiny::tags$section(class="rc-own",`data-rc-seat`=view$viewer,
      shiny::tags$h2(if(view$game_type=="holdem") own_name else paste0(own_name," / ",ui("Your cards",language)),
        if(view$viewer %in% view$dealer) table_dealer_tag(language)),
      shiny::tags$p(class="rc-own-details",paste(view$seats[[view$viewer]]$details,collapse=" | ")),
      own_cards,
      hand_controls))
}

table_button_content <- function(label,icon) {
  shiny::tagList(
    shiny::tags$span(class="rc-action-icon",`aria-hidden`="true",shiny::icon(icon)),
    shiny::tags$span(class="rc-action-label",label))
}

table_action_details <- function(game_type,action) {
  switch(game_type,
    holdem=switch(action,
      fold=c("xmark","Leave this hand."),
      check=c("check","Stay in the hand without betting."),
      call=c("equals","Match the current bet."),
      raise=c("arrow-up","Set a new total for this betting round.")),
    blackjack=switch(action,
      hit=c("plus","Draw another card."),
      stand=c("hand","Keep your current total."),
      double=c("arrow-up","Double the wager and draw one card."),
      split=c("clone","Split the pair into two hands.")),
    liar=switch(action,
      claim=c("bullhorn","Play the selected cards face down and declare their rank."),
      pass=c("forward-step","Decline to challenge this claim."),
      challenge=c("magnifying-glass","Challenge the latest claim.")))
}

table_action_tag <- function(view,action,version,label=NULL,disabled=FALSE) {
  language <- view$language
  details <- table_action_details(view$game_type,action)
  if(is.null(label)) {
    label <- action_label(action,language)
    if(action=="call") label <- paste(label,view$legal$to_call)
    if(length(details)) label <- paste0(toupper(substr(label,1L,1L)),substring(label,2L))
  }
  shiny::tags$button(type="button",class=paste("rc-action",if(action %in% c("fold","reject","challenge")) "rc-secondary"),
    `data-rc-action`=action,`data-rc-version`=version,disabled=if(disabled) "disabled",
    title=if(length(details)) ui(details[[2L]],language),
    if(length(details)) table_button_content(label,details[[1L]]) else label)
}

table_context_actions <- function(class,...) {
  shiny::tags$div(class=paste(class,"rc-context-controls"),shiny::tags$div(class="rc-actions",...))
}

table_blackjack_action_tag <- function(view,action,version,wager) {
  key <- switch(action,double="Double +%s",split="Split +%s")
  label <- if(!is.null(key)) ui(key,view$language,format(wager,trim=TRUE,scientific=FALSE))
  table_action_tag(view,action,version,label=label)
}

table_render_controls <- function(view,version,paused=FALSE) {
  language <- view$language
  if(view$hand_complete) return(shiny::tags$p(ui("Review the result, then continue or start a new table.",language)))
  if(paused) return(shiny::tags$p(role="status",ui("Resume play first",language)))
  if(!view$your_turn) return(shiny::tags$p(ui("Bots are playing",language)))
  legal <- view$legal
  if(view$game_type=="liar" && "claim" %in% legal$actions) {
    rank <- if(length(legal$ranks)==1L) shiny::tags$span(class="rc-claim-rank",ui("Rank %s",language,legal$ranks)) else
      shiny::selectInput("rank",ui("Declared rank",language),
        choices=c(stats::setNames("",ui("choose a rank",language)),stats::setNames(legal$ranks,legal$ranks)),
        selected="",selectize=FALSE)
    return(shiny::tags$div(class="rc-claim-row",rank,shiny::uiOutput("claim_control")))
  }
  shiny::tagList(
    if(view$selection_limit>0)
      shiny::tags$p(ui("Select your cards, then choose an action.",language)),
    if(any(c("raise","bet") %in% legal$actions))
      shiny::numericInput("amount",ui("Total stake",language),value=legal$min_stake,min=legal$min_stake,step=1),
    if(view$game_type=="scopa") shiny::uiOutput("capture_controls"),
    shiny::tags$div(class="rc-actions",lapply(legal$actions,function(action) table_action_tag(view,action,version))))
}

# Bind capture values to their selected hand position so an old radio value
# cannot silently choose an alternative for a newly selected card.
table_capture_index <- function(value,selected) {
  if(is.null(value) || length(value)!=1L || is.na(value) || length(selected)!=1L) return(NULL)
  parts <- strsplit(as.character(value),":",fixed=TRUE)[[1L]]
  if(length(parts)!=2L) return(NULL)
  numbers <- suppressWarnings(as.numeric(parts))
  if(anyNA(numbers) || any(!is.finite(numbers)) || any(numbers!=floor(numbers)) || numbers[1]!=selected) return(NULL)
  numbers[2]
}
