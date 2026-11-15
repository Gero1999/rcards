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
