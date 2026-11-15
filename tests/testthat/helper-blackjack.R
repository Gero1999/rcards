blackjack_pack <- function(codes) {
  suits <- c(s="Spades",h="Hearts",d="Diamonds",c="Clubs")
  cards <- lapply(strsplit(codes," ")[[1]],function(code) {
    value <- substr(code,1,nchar(code)-1L);if(value=="T") value<-"10"
    card(value,unname(suits[substr(code,nchar(code),nchar(code))]))
  })
  pack <- as_card_list(poker_pack());ids <- poker_card_ids(cards)
  c(cards,pack[!poker_card_ids(pack) %in% ids])
}
new_bj <- function(codes,...) new_blackjack(list(player("You",points=100)),
  rules=modifyList(blackjack_defaults(),list(decks=1L)),pack=blackjack_pack(codes),...)

