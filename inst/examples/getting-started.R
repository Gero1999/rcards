set.seed(42)
utils::data("deck_french", package = "cardgame", envir = environment())
pack <- cardgame::deck(cards = attr(deck_french, "main_cards"))
pack <- cardgame::shuffle(pack)
alice <- cardgame::player(name = "Alice")

dealt <- cardgame::take_cards(alice, pack, n = 5)
stopifnot(length(dealt$player$hand) == 5L, length(dealt$deck) == 47L)

# Return two cards to the bottom of the remaining deck.
returned <- cardgame::leave_cards(dealt$player, dealt$deck, c(1, 2))
stopifnot(length(returned$player$hand) == 3L, length(returned$deck) == 49L)
print(returned$player)
print(returned$deck)
