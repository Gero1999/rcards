test_that("Mus supports both king variants and score targets", {
  expect_equal(mus_rules()$kings,8)
  expect_equal(mus_rules()$points_to_win,40)
  expect_equal(mus_values(mus_hand(c(3,2,11,12)),mus_rules(8)),c(12,1,11,12))
  expect_equal(mus_values(mus_hand(c(3,2,11,12)),mus_rules(4)),c(3,2,11,12))
  expect_equal(mus_rules(4,30)$points_to_win,30)
  expect_error(mus_rules(6),"4 or 8")
  expect_error(mus_rules(points_to_win=35),"30 or 40")
})

test_that("unanimous mus exchanges preserve forty unique cards", {
  g <- new_mus(seed=4)
  for(i in 1:4) g <- mus_action(g,"mus")
  expect_equal(g$phase,"exchange")
  for(i in 1:4) g <- mus_action(g,"discard",cards=1:4,seed=4)
  expect_equal(g$phase,"mus")
  # Another exchange exhausts the stock and reuses prior discards.
  for(i in 1:4) g <- mus_action(g,"mus")
  for(i in 1:4) g <- mus_action(g,"discard",cards=1:4,seed=4)
  all_cards <- c(unlist(lapply(g$players,`[[`,"hand"),recursive=FALSE),as_card_list(g$deck),g$discard)
  expect_length(unique(mus_card_ids(all_cards)),40)
  expect_true(all(vapply(g$players,function(p) length(p$hand)==4L,logical(1))))
  expect_error(mus_action(g,"bet",amount=2),"Illegal")
  expect_error(mus_next_hand(g),"Finish")
})

test_that("hand comparisons and tie precedence follow configured rules", {
  info <- mus_hand_info(mus_hand(c(1,10,11,12)),mus_rules())
  expect_equal(info$juego_points,3)
  expect_gt(info$juego,mus_hand_info(mus_hand(c(12,11,10,7)),mus_rules())$juego)
  g <- new_mus(seed=1)
  # Four kings outrank two pairs; partnerships receive their own pair bonuses.
  expect_gt(mus_hand_info(Map(card,rep("12",4),c("Oros","Copas","Espadas","Bastos")),mus_rules())$pares,
            mus_hand_info(Map(card,c("12","12","11","11"),c("Oros","Copas","Espadas","Bastos")),mus_rules())$pares)
  g <- mus_action(g,"cut")
  g$players[[1]]$hand <- mus_hand(c(1,4,5,6),"Oros")
  g$players[[2]]$hand <- mus_hand(c(1,4,5,6),"Copas")
  g$players[[3]]$hand <- mus_hand(c(1,4,5,6),"Espadas")
  g$players[[4]]$hand <- mus_hand(c(1,4,5,6),"Bastos")
  expect_equal(mus_winner(g),1)
  g$mano <- 2
  expect_equal(mus_winner(g),2)
})

test_that("wagers, refusals, ordago and hand progression work", {
  g <- mus_action(new_mus(seed=5),"cut")
  expect_error(mus_action(g,"bet",amount=1),"stake")
  g <- mus_action(g,"bet",amount=2)
  g <- mus_action(g,"raise",amount=4)
  bettor <- g$bet_team
  g <- mus_action(g,"reject")
  expect_equal(g$scores[bettor],2)
  g <- finish_mus_passing(g)
  expect_true(g$hand_complete)
  next_g <- mus_next_hand(g,seed=6)
  expect_equal(next_g$scores,g$scores)
  expect_equal(next_g$mano,2)
  expect_equal(next_g$hand_number,2)
  allin <- mus_action(mus_action(new_mus(seed=5),"cut"),"ordago")
  allin <- mus_action(allin,"accept")
  expect_true(allin$complete)
  expect_equal(max(allin$scores),40)
})

test_that("games reach their target through complete hands", {
  g <- new_mus(rules=mus_rules(4,30),seed=12)
  for(i in 1:100) {
    g <- finish_mus_passing(g)
    if(g$complete) break
    g <- mus_next_hand(g,seed=i)
  }
  expect_true(g$complete)
  expect_true(max(g$scores)>=30)
})
