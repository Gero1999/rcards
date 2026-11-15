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

test_that("Blackjack doubles escrow the extra wager and settle exactly once", {
  g <- new_bj("Ts 9h 6d 8c 5c")
  g <- blackjack_step(g,"double")
  expect_equal(g$players[[1]]$points,120)
  expect_equal(g$results[[1]]$bet,20)
  expect_error(blackjack_step(g,"stand"),"Illegal")
  poor <- new_blackjack(list(player(points=10)),wager=10)
  if(!poor$complete) expect_false(any(c("double","split") %in% blackjack_legal(poor)$actions))
})

test_that("split aces receive one card and split 21 pays as ordinary 21", {
  g <- blackjack_step(new_bj("As 9h Ad 8c Ks Qh"),"split")
  expect_true(g$complete)
  expect_length(g$hands[[1]],2)
  expect_equal(g$players[[1]]$points,120)
  expect_false(any(vapply(g$results,`[[`,logical(1),"natural")))
  expect_equal(sum(vapply(g$results,`[[`,0,"returned")),40)
})

test_that("soft seventeen payout and split limits follow configured rules", {
  pack <- blackjack_pack("Ts Ah 8d 6c 2s")
  start <- function(hit) new_blackjack(list(player(points=100)),
    rules=blackjack_rules(decks=1,hit_soft17=hit),pack=pack)
  expect_equal(blackjack_step(start(FALSE),"stand")$players[[1]]$points,110)
  expect_equal(blackjack_step(start(TRUE),"stand")$players[[1]]$points,90)
  g <- new_blackjack(list(player(points=100)),rules=blackjack_rules(decks=1,blackjack_payout=1.2),
    pack=blackjack_pack("As 9h Kd 8c"))
  expect_equal(g$players[[1]]$points,112)
  g <- new_blackjack(list(player(points=100)),rules=blackjack_rules(decks=1,max_hands=1),
    pack=blackjack_pack("8s 9h 8d 8c"))
  expect_false("split" %in% blackjack_legal(g)$actions)
  expect_error(blackjack_rules(hit_soft17=NA),"flags")
})
