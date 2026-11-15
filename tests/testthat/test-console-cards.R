test_that("card art wraps, indexes discards, and supports ASCII", {
  cards <- as_card_list(poker_pack())[1:5]
  rows <- console_card_rows(cards,"ascii",width=16,numbered=TRUE)
  expect_true(all(nchar(rows,type="width")<=16))
  expect_match(paste(rows,collapse="\n"),"\\[5\\]")
  expect_true(all(utf8ToInt(paste(rows,collapse=""))<128))
  expect_match(paste(console_card_rows(cards[1],"unicode"),collapse=""),"\u250c")
  expect_equal(console_card_rows(list()),"(none)")
})

test_that("showdown cards are revealed only when rules require", {
  controller <- console_player(style="ascii",write=function(text) NULL)
  h <- holdem_action(new_holdem(seed=3),"fold")
  text <- paste(console_hand_end_text(h,controller),collapse="\n")
  expect_match(text,"wins")
  expect_match(text,"back \\(uncalled\\)")
  expect_false(grepl("SHOWDOWN|\\+-----\\+",text))
  complete <- finish_calling(new_holdem(seed=3))
  expect_equal(console_revealed_hands(complete),1:2)
  expect_match(paste(console_hand_end_text(complete,controller),collapse="\n"),"SHOWDOWN")
  g <- mus_action(new_mus(seed=3),"cut")
  # Set the betting partnership one point from victory, then refuse.
  g$scores[] <- 39L
  g <- mus_action(mus_action(g,"bet",amount=2),"reject")
  expect_true(g$complete)
  expect_length(console_revealed_hands(g),0)
  finished <- finish_mus_passing(new_mus(seed=3))
  expect_equal(console_revealed_hands(finished),1:4)
})

test_that("automatic simulation has no presentation side effects", {
  players <- list(player("A",points=20),player("B",points=20))
  expect_output(simulate_poker_game(players,rep(list(passive_controller),2),seed=1),NA)
  expect_output(play(new_mus(seed=2),rep(list(passive_mus_controller),4)),NA)
})
