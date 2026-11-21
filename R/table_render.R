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
  lapply(view$center,table_render_zone,version=version,highlight=highlight,language=language)
}

table_render_view <- function(view,version,hand_controls=NULL,pile_controls=NULL) {
  language <- view$language
  opponents <- view$seats[!vapply(view$seats,`[[`,logical(1),"viewer")]
  shiny::tags$section(class="rc-table",`aria-label`=ui("Card table",language),
    shiny::tags$div(class="rc-opponents",lapply(opponents,function(player) {
      shiny::tags$section(class=paste("rc-seat",if(player$active) "rc-active-seat"),
        shiny::tags$h2(player$name),shiny::tags$p(paste(player$details,collapse=" | ")),
        lapply(player$hands,table_render_zone,version=version,language=language))
    })),
    shiny::tags$div(class="rc-center",
      shiny::tags$h1(class="rc-game-title",names(table_game_names())[match(view$game_type,table_game_names())],
        if(!is.null(view$phase)) shiny::tags$small(ui(toupper(view$phase),language))),lapply(view$status,function(line) shiny::tags$p(class="rc-status",line)),
      shiny::uiOutput("table_center_cards"),
      pile_controls,
      if(length(view$history)) shiny::tags$details(class="rc-history",shiny::tags$summary(ui("Recent actions",language)),
        shiny::tags$ol(lapply(view$history,shiny::tags$li)))),
    shiny::tags$section(class="rc-own",shiny::tags$h2(paste0(view$seats[[view$viewer]]$name," / ",ui("Your cards",language))),
      shiny::tags$p(class="rc-own-details",paste(view$seats[[view$viewer]]$details,collapse=" | ")),
      shiny::uiOutput("table_own_cards",class="rc-own-zones"),
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

table_render_controls <- function(view,version,paused=FALSE) {
  language <- view$language
  if(view$hand_complete) return(shiny::tags$p(ui("Review the result, then continue or start a new table.",language)))
  if(paused) return(shiny::tags$p(role="status",ui("Resume play first",language)))
  if(!view$your_turn) return(shiny::tags$p(ui("Bots are playing",language)))
  legal <- view$legal
  shiny::tagList(
    if(view$selection_limit>0 && view$game_type!="liar")
      shiny::tags$p(ui("Select your cards, then choose an action.",language)),
    if(any(c("raise","bet") %in% legal$actions)) {
      poker <- view$game_type=="holdem"
      minimum <- if(poker) legal$min_raise_to else legal$min_stake
      shiny::numericInput("amount",ui(if(poker) "Raise TO total this betting round" else "Total stake",language),
        value=minimum,min=minimum,max=if(poker) legal$max_raise_to else NA,step=1)
    },
    if(view$game_type=="holdem" && "raise" %in% legal$actions)
      shiny::tags$p(class="rc-help",ui("Total includes chips already in this round; enter the new total, not extra chips.",language)),
    if(view$game_type=="liar" && "claim" %in% legal$actions)
      shiny::selectInput("rank",ui("Declared rank",language),choices=if(length(legal$ranks)>1L)
        c(stats::setNames("",ui("choose a rank",language)),stats::setNames(legal$ranks,legal$ranks)) else legal$ranks,
        selected=if(length(legal$ranks)==1L) legal$ranks else "",selectize=FALSE),
    if(view$game_type=="scopa") shiny::uiOutput("capture_controls"),
    shiny::tags$div(class="rc-actions",lapply(legal$actions,function(action) {
      label <- action_label(action,language)
      if(action=="call") label <- paste(label,legal$to_call)
      details <- table_action_details(view$game_type,action)
      if(length(details)) label <- paste0(toupper(substr(label,1L,1L)),substring(label,2L))
      shiny::tags$button(type="button",class=paste("rc-action",if(action %in% c("fold","reject","challenge")) "rc-secondary"),
        `data-rc-action`=action,`data-rc-version`=version,
        title=if(length(details)) ui(details[[2L]],language),
        if(length(details)) table_button_content(label,details[[1L]]) else label)
    })))
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
