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
