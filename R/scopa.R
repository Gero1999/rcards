scopa_defaults <- function() list(target=11L)

scopa_sides <- function(n) if(n==4L) c(1L,2L,1L,2L) else seq_len(n)

scopa_deal_batch <- function(g) {
  n <- length(g$players)
  if(any(vapply(g$players,function(p) length(p$hand),0L))) stop("Finish all hands before dealing")
  if(length(g$stock)<3L*n) stop("Insufficient Scopa stock")
  order <- clockwise_seats(n,g$dealer)
  for(round in 1:3) for(i in order) {
    g$players[[i]]$hand <- c(g$players[[i]]$hand,g$stock[1L])
    g$stock <- g$stock[-1L]
  }
  g$actor <- order[1L]
  g
}

new_scopa <- function(players=c("You","Bot"),rules=scopa_defaults(),dealer=length(players),seed=NULL,pack=NULL) {
  rules <- do.call(scopa_rules,rules)
  if(is.character(players)) players <- lapply(players,player)
  n <- length(players)
  if(!is.list(players) || !n %in% 2:4 || !all(vapply(players,inherits,logical(1),"player"))) stop("Scopa needs two three or four players")
  names <- vapply(players,`[[`,"","name")
  if(anyNA(names) || any(!nzchar(names)) || anyDuplicated(names)) stop("Player names must be distinct")
  check_number(dealer,"dealer",1,n,TRUE)
  check_number(rules$target,"target",1,integer=TRUE)
  if(is.null(pack)) pack <- with_poker_seed(seed,function() shuffle(italian_deck()))
  if(length(italian_card_ids(pack))!=40L) stop("Scopa needs a complete forty-card Italian pack")
  for(i in seq_len(n)) players[[i]]$hand <- list()
  sides <- scopa_sides(n)
  g <- structure(list(players=players,teams=sides,scores=numeric(max(sides)),rules=rules,
    dealer=dealer,actor=NULL,stock=as_card_list(pack),table_cards=list(),
    captured=rep(list(list()),n),sweeps=integer(n),last_capturer=NULL,
    hand_number=1L,hand_complete=FALSE,complete=FALSE,history=list(),results=list(),winner=NULL),
    class=c("scopa_game","game"))
  g <- scopa_deal_batch(g)
  g$table_cards <- g$stock[1:4];g$stock <- g$stock[-(1:4)]
  g
}

scopa_capture_options <- function(cards,value) {
  values <- as.integer(vapply(cards,`[[`,"","value"))
  singles <- which(values==value)
  if(length(singles)) return(lapply(singles,as.integer))
  options <- list()
  search <- function(start,remaining,chosen=integer()) {
    if(remaining==0L) {options[[length(options)+1L]] <<- chosen;return(invisible(NULL))}
    if(start>length(values)) return(invisible(NULL))
    for(i in seq.int(start,length(values))) if(values[i]<=remaining)
      search(i+1L,remaining-values[i],c(chosen,i))
    invisible(NULL)
  }
  search(1L,value)
  if(length(options)) options else list(integer())
}

scopa_legal <- function(g) {
  if(g$hand_complete || g$complete) return(list(actor=NULL,actions=character()))
  hand <- g$players[[g$actor]]$hand
  list(actor=g$actor,actions="play",card_positions=seq_along(hand),
    captures=lapply(hand,function(card) scopa_capture_options(g$table_cards,as.integer(card$value))))
}

scopa_step <- function(g,position,capture=NULL) {
  if(g$hand_complete || g$complete) stop("Scopa deal is complete")
  legal <- scopa_legal(g)
  check_number(position,"card position",1,length(legal$captures),TRUE)
  options <- legal$captures[[position]]
  if(is.null(capture)) {
    if(length(options)>1L) stop("Choose a capture option")
    capture <- 1L
  }
  check_number(capture,"capture option",1,length(options),TRUE)
  take <- options[[capture]];i <- g$actor
  played <- g$players[[i]]$hand[[position]]
  final_play <- !length(g$stock) && sum(vapply(g$players,function(p) length(p$hand),0L))==1L
  g$players[[i]]$hand <- g$players[[i]]$hand[-position]
  sweep <- FALSE
  taken <- g$table_cards[take]
  if(length(take)) {
    g$captured[[i]] <- c(g$captured[[i]],list(played),taken)
    g$table_cards <- g$table_cards[-take]
    g$last_capturer <- i
    sweep <- !length(g$table_cards) && !final_play
    if(sweep) g$sweeps[i] <- g$sweeps[i]+1L
  } else g$table_cards <- c(g$table_cards,list(played))
  g$history[[length(g$history)+1L]] <- list(player=i,action="play",card=played,taken=taken,sweep=sweep)
  counts <- vapply(g$players,function(p) length(p$hand),0L)
  if(!any(counts)) {
    if(length(g$stock)) return(scopa_deal_batch(g))
    return(scopa_close_deal(g))
  }
  order <- clockwise_seats(length(g$players),i)
  g$actor <- order[order %in% which(counts>0)][1L]
  g
}

scopa_close_deal <- function(g) {
  if(!is.null(g$last_capturer)) {
    g$captured[[g$last_capturer]] <- c(g$captured[[g$last_capturer]],g$table_cards)
    g$table_cards <- list()
  }
  g$hand_complete <- TRUE;g$actor <- NULL
  scopa_settle(g)
}
