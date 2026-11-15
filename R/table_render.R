table_card_tag <- function(card,index,zone,version,selectable=FALSE,selected=FALSE,highlight=FALSE,language="en",indexed=selectable) {
  classes <- paste("rc-card",if(card$back) "rc-back" else if(card$suit %in% c("Hearts","Diamonds")) "rc-red" else "rc-black",
    if(selectable) "rc-selectable",if(selected) "rc-selected",if(highlight) "rc-highlight")
  label <- if(card$back) ui("Covered card",language) else paste0(index,": ",card$label)
  content <- if(card$back) shiny::tags$span(class="rc-back-mark","rc") else shiny::tagList(
    shiny::tags$span(class="rc-rank",card$value),shiny::tags$span(class="rc-suit",card$symbol),
    shiny::tags$span(class="rc-rank rc-bottom",card$value),
    if(indexed) shiny::tags$span(class="rc-index",index))
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

table_render_view <- function(view,selected=integer(),capture=NULL,version=0L) {
  language <- view$language;highlight <- integer()
  if(view$game_type=="scopa" && view$your_turn && length(selected)==1L) {
    index <- table_capture_index(capture,selected);options<-view$legal$captures[[selected]]
    if(!is.null(index) && index>=1 && index<=length(options)) highlight <- options[[index]]
  }
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
      lapply(view$center,table_render_zone,version=version,highlight=highlight,language=language),
      if(length(view$history)) shiny::tags$details(class="rc-history",shiny::tags$summary(ui("Recent actions",language)),
        shiny::tags$ol(lapply(view$history,shiny::tags$li)))),
    shiny::tags$section(class="rc-own",shiny::tags$h2(paste0(view$seats[[view$viewer]]$name," / ",ui("Your cards",language))),
      shiny::tags$p(class="rc-own-details",paste(view$seats[[view$viewer]]$details,collapse=" | ")),
      lapply(view$own,table_render_zone,version=version,selected=selected,language=language)))
}

table_render_controls <- function(view,version,paused=FALSE) {
  language <- view$language
  if(view$hand_complete) return(shiny::tags$p(ui("Review the result, then start a new table or next hand.",language)))
  if(!view$your_turn) return(shiny::tags$p(ui("Bots are playing",language)))
  legal <- view$legal
  shiny::tagList(
    if(view$selection_limit>0) shiny::tags$p(ui("Select your cards, then choose an action.",language)),
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
      shiny::tags$button(type="button",class=paste("rc-action",if(action %in% c("fold","reject","challenge")) "rc-secondary"),
        `data-rc-action`=action,`data-rc-version`=version,disabled=if(paused) "disabled" else NULL,
        paste(action_label(action,language),if(action=="call") legal$to_call else ""))
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
