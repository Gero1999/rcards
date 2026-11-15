#' Configure Mentiroso (Liar) rules
#' @param deck Spanish forty-card or French fifty-two-card pack, without jokers.
#' @param max_cards Maximum cards in one claim, one through four.
#' @return A liar_rules object. The rank stays fixed until a challenge; the
#' pickup player starts the next pile. See docs/rules/liar.md.
#' @export
liar_rules <- function(deck=c("spanish","french"),max_cards=4L) {
  deck <- match.arg(deck)
  check_number(max_cards,"max_cards",1,4,TRUE)
  structure(list(deck=deck,max_cards=max_cards),class="liar_rules")
}

liar_ranks <- function(rules) if(rules$deck=="spanish") c(as.character(1:7),"10","11","12") else c("A",as.character(2:10),"J","Q","K")

liar_pack <- function(rules) if(rules$deck=="spanish") as_card_list(mus_pack()) else as_card_list(poker_pack())

liar_card_ids <- function(cards,rules) {
  if(rules$deck=="spanish") mus_card_ids(cards) else poker_card_ids(cards)
}

new_liar <- function(players=c("You","Bot 1","Bot 2"),rules=liar_rules(),dealer=length(players),seed=NULL,pack=NULL) {
  rules <- do.call(liar_rules,rules)
  if(is.character(players)) players <- lapply(players,player)
  n <- length(players)
  if(!is.list(players) || !n %in% 2:8 || !all(vapply(players,inherits,logical(1),"player"))) stop("Liar needs two through eight players")
  names <- vapply(players,`[[`,"","name")
  if(anyNA(names) || any(!nzchar(names)) || anyDuplicated(names)) stop("Player names must be distinct")
  check_number(dealer,"dealer",1,n,TRUE)
  expected <- liar_pack(rules)
  if(is.null(pack)) pack <- with_poker_seed(seed,function() expected[sample.int(length(expected))])
  ids <- liar_card_ids(pack,rules)
  if(!identical(sort(ids),sort(liar_card_ids(expected,rules)))) stop("Liar requires one complete configured pack")
  for(i in seq_len(n)) players[[i]]$hand <- list()
  order <- clockwise_seats(n,dealer)
  for(j in seq_along(pack)) {
    i <- order[((j-1L) %% n)+1L]
    players[[i]]$hand <- c(players[[i]]$hand,list(pack[[j]]))
  }
  structure(list(players=players,rules=rules,dealer=dealer,actor=order[1L],phase="claim",
    rank=NULL,pile=list(),last_claim=NULL,responders=integer(),last_reveal=NULL,
    history=list(),complete=FALSE,winner=NULL),class=c("liar_game","game"))
}

liar_assert_cards <- function(g) {
  cards <- c(g$pile,unlist(lapply(g$players,`[[`,"hand"),recursive=FALSE))
  if(!identical(sort(liar_card_ids(cards,g$rules)),sort(liar_card_ids(liar_pack(g$rules),g$rules))))
    stop("Liar card conservation failed")
  invisible(g)
}

liar_legal <- function(g) {
  if(g$complete) return(list(actor=NULL,actions=character()))
  if(g$phase=="response") return(list(actor=g$actor,actions=c("challenge","pass")))
  list(actor=g$actor,actions="claim",card_positions=seq_along(g$players[[g$actor]]$hand),
    ranks=if(is.null(g$rank)) liar_ranks(g$rules) else g$rank,
    max_cards=min(g$rules$max_cards,length(g$players[[g$actor]]$hand)))
}

liar_finish <- function(g,winner) {
  g$complete <- TRUE;g$winner <- winner;g$actor <- NULL;g$phase <- "complete"
  g$responders <- integer()
  g
}

liar_step <- function(g,action,cards=integer(),rank=NULL) {
  legal <- liar_legal(g)
  if(!is.character(action) || length(action)!=1L || is.na(action) || !action %in% legal$actions)
    stop("Illegal Liar action")
  i <- g$actor
  if(action=="claim") {
    validate_positions(cards,length(g$players[[i]]$hand))
    if(!length(cards) || length(cards)>legal$max_cards) stop("Select one through the allowed maximum cards")
    if(is.null(rank)) rank <- g$rank
    if(length(rank)!=1L || is.na(rank) || !is.atomic(rank)) stop("Choose a claim rank")
    rank <- toupper(as.character(rank))
    if(!rank %in% legal$ranks) stop("Claim the current declared rank")
    batch <- g$players[[i]]$hand[cards]
    g$players[[i]]$hand <- g$players[[i]]$hand[-cards]
    g$pile <- c(g$pile,batch);g$rank <- rank
    g$last_claim <- list(player=i,rank=rank,count=length(batch),cards=batch)
    g$last_reveal <- NULL
    g$history[[length(g$history)+1L]] <- list(action="claim",player=i,rank=rank,count=length(batch))
    g$phase <- "response"
    g$responders <- clockwise_seats(length(g$players),i)[seq_len(length(g$players)-1L)]
    g$actor <- g$responders[1L]
  } else {
    if(length(cards) || !is.null(rank)) stop("Responses take no cards or ranks")
    claim <- g$last_claim
    if(action=="pass") {
      g$history[[length(g$history)+1L]] <- list(action="pass",player=i)
      g$responders <- g$responders[-1L]
      if(length(g$responders)) g$actor <- g$responders[1L] else {
        if(!length(g$players[[claim$player]]$hand)) return(liar_finish(g,claim$player))
        g$actor <- clockwise_seats(length(g$players),claim$player)[1L]
        g$phase <- "claim";g$last_claim <- NULL
      }
    } else {
      caught <- any(vapply(claim$cards,function(x) x$value!=claim$rank,logical(1)))
      picker <- if(caught) claim$player else i
      size <- length(g$pile)
      g$players[[picker]]$hand <- c(g$players[[picker]]$hand,g$pile)
      g$pile <- list()
      reveal <- list(action="challenge",player=i,claimant=claim$player,rank=claim$rank,
        caught=caught,picker=picker,pickup_count=size,cards=claim$cards)
      g$last_reveal <- reveal;g$history[[length(g$history)+1L]] <- reveal
      g$last_claim <- NULL;g$responders <- integer();g$rank <- NULL
      if(!length(g$players[[claim$player]]$hand)) return(liar_finish(g,claim$player))
      g$actor <- picker;g$phase <- "claim"
    }
  }
  liar_assert_cards(g)
  g
}
