# Compact public readouts for the browser. The renderer-neutral view keeps its
# existing status text for callers and for the expandable table details.
table_metric_tag <- function(label,value,icon=NULL) {
  shiny::tags$span(class="rc-table-metric",
    if(!is.null(icon)) shiny::tags$span(class="rc-table-metric-icon",`aria-hidden`="true",shiny::icon(icon)),
    shiny::tags$span(class="rc-table-metric-label",label),
    shiny::tags$strong(class="rc-table-metric-value",value))
}

table_scene_summary <- function(view,observation) {
  language <- view$language
  view$table_details <- view$status
  view$status <- character()

  if(view$game_type %in% c("mus","scopa")) {
    sides <- seq_along(observation$scores)
    scores <- lapply(sides,function(i) table_metric_tag(ui("Side %s",language,i),observation$scores[[i]]))
    for(i in seq_along(view$seats))
      view$seats[[i]]$details <- ui("Side %s",language,observation$teams[[i]])
    if(view$game_type=="scopa") {
      view$metrics <- c(list(table_metric_tag(ui("Stock",language),observation$stock_count,"layer-group")),scores)
    } else {
      stake <- if(isTRUE(observation$ordago)) ui("ORDAGO",language) else observation$wager
      betting <- isTRUE(observation$ordago) || isTRUE(observation$wager>0)
      view$metrics <- c(scores,if(betting) list(table_metric_tag(ui("Total stake",language),stake,"coins")))
    }
  }
  if(view$game_type=="liar") {
    view$metrics <- list(table_metric_tag(ui("Pile cards",language),observation$pile_count,"layer-group"),
      if(!is.null(observation$rank)) table_metric_tag(ui("Declared rank",language),observation$rank),
      if(!is.null(observation$claim)) table_metric_tag(ui("Claimed cards",language),observation$claim$count))
    if(!is.null(observation$claim))
      view$metric_caption <- ui("Claimed by %s",language,observation$player_names[[observation$claim$player]])
  }
  view
}
