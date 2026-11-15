eq_hand <- function(text) {
  suits <- c(s="Spades",h="Hearts",d="Diamonds",c="Clubs")
  lapply(strsplit(text," ")[[1]],function(x) card(if(substr(x,1,1)=="T") "10" else substr(x,1,1),unname(suits[substr(x,2,2)])))
}

test_that("exact poker equity handles board ties and known opposing hands", {
  result <- poker_equity(eq_hand("2h 3d"),eq_hand("Ts Js Qs Ks As"),method="exact")
  expect_equal(result$method,"exact")
  expect_equal(result$estimates$estimate,c(0,1,0,0.5))
  expect_true(all(result$estimates$standard_error==0))
  result <- poker_equity(eq_hand("As Ah"),eq_hand("2s 4h 7d 9c Js"),
    opponent_hands=list(eq_hand("Ks Kh")),method="exact")
  expect_equal(result$estimates$estimate,c(1,0,0,1))
  expect_equal(result$possible_states,1)
  expect_error(poker_equity(eq_hand("As Ah"),method="exact",max_exact=10),"exceeds")
  expect_error(poker_equity(eq_hand("As Ah"),opponent_hands=list(eq_hand("As Kh"))),"Duplicate")
})

test_that("sampled poker estimates preserve RNG and agree with small exact spaces", {
  set.seed(9);before <- .Random.seed
  args <- list(hole=eq_hand("As Ah"),board=eq_hand("2s 4h 7d 9c"),opponent_hands=list(eq_hand("Ks Kh")))
  exact <- do.call(poker_equity,c(args,list(method="exact")))
  sampled <- do.call(poker_equity,c(args,list(method="sample",n=2000,seed=1)))
  expect_identical(.Random.seed,before)
  expect_lt(abs(exact$estimates$estimate[1]-sampled$estimates$estimate[1]),0.05)
  expect_equal(sum(sampled$estimates$estimate[1:3]),1)
  g <- new_holdem(seed=1);o <- game_observation(g)
  expect_equal(estimate_hand(o,n=10,seed=1)$type,"poker_showdown")
})
