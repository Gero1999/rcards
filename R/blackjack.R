# Internal engine; public protocol and exports are added after complete rules.
blackjack_defaults <- function() list(decks=6L,hit_soft17=FALSE,blackjack_payout=1.5,
  double_after_split=TRUE,max_hands=4L)

blackjack_total <- function(cards) {
  values <- vapply(cards,function(x) if(x$value=="A") 11 else if(x$value %in% c("J","Q","K")) 10 else as.numeric(x$value),0)
  total <- sum(values); aces <- sum(values==11)
  while(total>21 && aces>0) { total <- total-10; aces <- aces-1L }
  list(total=total,soft=aces>0,bust=total>21,natural=length(cards)==2L && total==21)
}

blackjack_draw <- function(g) {
  if(!length(g$shoe)) stop("Blackjack shoe exhausted")
  list(card=g$shoe[[1L]],shoe=g$shoe[-1L])
}

blackjack_take <- function(g,seat=0L,hand=1L) {
  draw <- blackjack_draw(g);g$shoe <- draw$shoe
  if(seat==0L) g$dealer <- c(g$dealer,list(draw$card)) else
    g$hands[[seat]][[hand]]$cards <- c(g$hands[[seat]][[hand]]$cards,list(draw$card))
  g
}

new_blackjack <- function(players,wager=10,rules=blackjack_defaults(),seed=NULL,pack=NULL) {
  rules <- do.call(blackjack_rules,rules)
  if(!is.list(players) || !length(players) %in% 1:7 ||
     !all(vapply(players,inherits,logical(1),"player"))) stop("Expected one to seven players")
  if(length(wager)==1L) wager <- rep(wager,length(players))
  if(!is.numeric(wager) || length(wager)!=length(players) || anyNA(wager) ||
     any(!is.finite(wager)) || any(wager<=0)) stop("Provide positive wagers for all players")
  for(i in seq_along(players)) {
    check_number(players[[i]]$points,"stack",wager[i])
    players[[i]]$hand <- list()
  }
  if(is.null(pack)) {
    pack <- with_poker_seed(seed,function() {
      cards <- rep(as_card_list(poker_pack()),rules$decks)
      cards[sample.int(length(cards))]
    })
  }
  ids <- vapply(pack,function(x) poker_card_ids(list(x)),"")
  expected <- rep(poker_card_ids(poker_pack()),rules$decks)
  if(!identical(sort(ids),sort(expected))) stop("Pack does not match configured Blackjack decks")
  g <- structure(list(players=players,initial_stacks=vapply(players,`[[`,0,"points"),
    rules=rules,shoe=as_card_list(pack),dealer=list(),hands=lapply(wager,function(bet)
      list(list(cards=list(),bet=bet,state="active",split=FALSE,split_aces=FALSE))),
    actor=1L,active_hand=1L,history=list(),complete=FALSE,results=list()),class=c("blackjack_game","game"))
  for(i in seq_along(players)) g$players[[i]]$points <- g$players[[i]]$points-wager[i]
  for(round in 1:2) {
    for(i in seq_along(players)) g <- blackjack_take(g,i)
    g <- blackjack_take(g)
  }
  for(i in seq_along(players)) if(blackjack_total(g$hands[[i]][[1]]$cards)$natural)
    g$hands[[i]][[1]]$state <- "natural"
  if(blackjack_total(g$dealer)$natural) return(blackjack_finish(g))
  blackjack_advance(g)
}

blackjack_legal <- function(g) {
  if(g$complete) return(list(actor=NULL,actions=character()))
  h <- g$hands[[g$actor]][[g$active_hand]]
  funded <- g$players[[g$actor]]$points>=h$bet
  double <- length(h$cards)==2L && funded && (!h$split || g$rules$double_after_split) && !h$split_aces
  split <- length(h$cards)==2L && funded && length(g$hands[[g$actor]])<g$rules$max_hands &&
    h$cards[[1]]$value==h$cards[[2]]$value && !h$split_aces
  list(actor=g$actor,hand=g$active_hand,actions=c("hit","stand",if(double) "double",if(split) "split"))
}

blackjack_step <- function(g,action) {
  if(length(action)!=1L || is.na(action) || !action %in% blackjack_legal(g)$actions) stop("Illegal Blackjack action")
  i <- g$actor;j <- g$active_hand
  if(action=="split") {
    original <- g$hands[[i]][[j]]
    g$players[[i]]$points <- g$players[[i]]$points-original$bet
    aces <- original$cards[[1]]$value=="A"
    hands <- lapply(original$cards,function(x) list(cards=list(x),bet=original$bet,
      state="active",split=TRUE,split_aces=aces))
    before <- if(j>1L) g$hands[[i]][seq_len(j-1L)] else list()
    after <- if(j<length(g$hands[[i]])) g$hands[[i]][seq.int(j+1L,length(g$hands[[i]]))] else list()
    g$hands[[i]] <- c(before,hands,after)
    for(k in j:(j+1L)) {
      g <- blackjack_take(g,i,k)
      if(aces || blackjack_total(g$hands[[i]][[k]]$cards)$total==21)
        g$hands[[i]][[k]]$state <- "stood"
    }
  } else if(action=="double") {
    bet <- g$hands[[i]][[j]]$bet
    g$players[[i]]$points <- g$players[[i]]$points-bet
    g$hands[[i]][[j]]$bet <- bet*2
    g <- blackjack_take(g,i,j)
    g$hands[[i]][[j]]$state <- if(blackjack_total(g$hands[[i]][[j]]$cards)$bust) "bust" else "stood"
  } else if(action=="hit") {
    g <- blackjack_take(g,i,j)
    total <- blackjack_total(g$hands[[i]][[j]]$cards)
    if(total$bust) g$hands[[i]][[j]]$state <- "bust" else if(total$total==21) g$hands[[i]][[j]]$state <- "stood"
  } else g$hands[[i]][[j]]$state <- "stood"
  g$history[[length(g$history)+1L]] <- list(player=i,hand=j,action=action)
  blackjack_advance(g)
}

blackjack_advance <- function(g) {
  for(i in seq_along(g$hands)) for(j in seq_along(g$hands[[i]])) {
    if(g$hands[[i]][[j]]$state=="active") {g$actor<-i;g$active_hand<-j;return(g)}
  }
  blackjack_finish(g)
}

blackjack_finish <- function(g) {
  dealer <- blackjack_total(g$dealer)
  live <- any(vapply(unlist(g$hands,recursive=FALSE),function(h) h$state %in% c("active","stood"),logical(1)))
  if(live && !dealer$natural) repeat {
    if(dealer$total>17 || (dealer$total==17 && (!dealer$soft || !g$rules$hit_soft17))) break
    g <- blackjack_take(g);dealer <- blackjack_total(g$dealer)
  }
  for(i in seq_along(g$hands)) for(j in seq_along(g$hands[[i]])) {
    h <- g$hands[[i]][[j]];total <- blackjack_total(h$cards)
    natural <- isTRUE(total$natural) && !h$split
    outcome <- if(total$bust) "loss" else if(dealer$natural) {
      if(natural) "push" else "loss"
    } else if(natural || dealer$bust || total$total>dealer$total) "win" else if(total$total==dealer$total) "push" else "loss"
    returned <- if(outcome=="push") h$bet else if(outcome=="loss") 0 else h$bet*(1+if(natural) g$rules$blackjack_payout else 1)
    g$players[[i]]$points <- g$players[[i]]$points+returned
    g$results[[length(g$results)+1L]] <- list(player=i,hand=j,outcome=outcome,bet=h$bet,returned=returned,natural=natural,total=total$total)
  }
  g$complete <- TRUE;g$actor <- NULL;g$active_hand <- NULL
  g
}
