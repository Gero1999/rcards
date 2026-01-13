# Define suit symbols (optional, for display)
suit_symbols <- c(
  Oros = "\U0001F4B0",      # (Coins)
  Copas = "\U0001F377",     # (Cups)
  Espadas = "\U0001F5E1",   # (Swords)
  Bastos = "\U0001F528"     # (Clubs)
)

# Spanish deck values
base_values <- c("1", "2", "3", "4", "5", "6", "7", "10", "11", "12")
optional_values <- c("8", "9")
suits <- c("Oros", "Copas", "Espadas", "Bastos")

# Create 40 base cards
base_cards <- unlist(lapply(suits, function(suit) {
  lapply(base_values, function(value) {
    card(value, suit)
  })
}), recursive = FALSE)

# Create optional 8s and 9s
optional_cards <- unlist(lapply(suits, function(suit) {
  lapply(optional_values, function(value) {
    card(value, suit)
  })
}), recursive = FALSE)

# Create the Spanish deck with optional 8s and 9s
deck_spanish <- deck(cards = base_cards, optional_cards = optional_cards, language = "es")

# Export the deck using usethis::use_data
usethis::use_data(deck_spanish, overwrite = TRUE)
