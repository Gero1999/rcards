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

test_that("Blackjack handles aces naturals pushes and dealer play", {
  expect_equal(blackjack_total(list(card("A","Spades"),card("A","Hearts"),card("9","Clubs")))$total,21)
  g <- new_bj("As 9h Kd 8c")
  expect_true(g$complete);expect_equal(g$players[[1]]$points,115)
  g <- new_bj("As Ah Kd Kc")
  expect_equal(g$results[[1]]$outcome,"push");expect_equal(g$players[[1]]$points,100)
  g <- new_bj("Ts 9h 8d 8c")
  expect_equal(blackjack_step(g,"stand")$players[[1]]$points,110)
  g <- new_bj("Ts 9h 8d 8c Kc")
  busted <- blackjack_step(g,"hit")
  expect_equal(busted$players[[1]]$points,90)
  expect_error(blackjack_step(g,"unknown"),"Illegal")
  expect_false(g$complete)
})

test_that("Blackjack construction is seeded and validates complete shoes", {
  set.seed(12);before <- .Random.seed
  a <- new_blackjack(list(player(points=100)),seed=4)
  expect_identical(.Random.seed,before)
  expect_identical(a,new_blackjack(list(player(points=100)),seed=4))
  expect_error(new_blackjack(list(player(points=100)),pack=list()),"Pack")
})
