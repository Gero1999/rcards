test_that("known unbeatable hands and forced board ties have exact equity", {
  hole <- Map(card,c("A","K"),rep("Spades",2))
  board <- Map(card,c("Q","J","10","2","3"),c("Spades","Spades","Spades","Hearts","Clubs"))
  won <- holdem_equity(hole,board,opponents=3,equity_samples=16,seed=42)
  expect_equal(won$equity,1)
  expect_equal(won$win,1)
  board <- Map(card,c("A","K","Q","J","10"),rep("Spades",5))
  hole <- Map(card,c("2","3"),rep("Hearts",2))
  tied <- holdem_equity(hole,board,opponents=2,equity_samples=16,seed=42)
  expect_equal(tied$equity,1/3)
  expect_equal(tied$tie,1)
  expect_equal(tied$win+tied$tie+tied$loss,1)
})

test_that("equity estimates reproduce without changing caller RNG state", {
  hole <- list(card("A","Spades"),card("A","Hearts"))
  set.seed(1); before <- .Random.seed
  a <- holdem_equity(hole,equity_samples=64,seed=20)
  expect_identical(.Random.seed,before)
  expect_identical(a,holdem_equity(hole,equity_samples=64,seed=20))
  expect_gt(a$equity,0.5)
  expect_error(holdem_equity(hole,board=hole,equity_samples=2),"legal board")
  expect_error(holdem_equity(hole,list(hole[[1]],card("2","Hearts"),card("3","Hearts"))),"Duplicate")
  expect_error(holdem_equity(hole,equity_samples=0),"equity_samples")
})

test_that("fast seven-card scoring agrees with exhaustive five-card selection", {
  set.seed(12)
  pack <- poker_pack()
  for(i in 1:40) {
    hand <- as_card_list(pack)[sample.int(52,7)]
    exhaustive <- max(apply(utils::combn(7,5),2,function(ix) poker_five_score(hand[ix])))
    expect_equal(poker_hand_points(hand),exhaustive)
  }
})
