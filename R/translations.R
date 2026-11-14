#' Translate a card display into English or Spanish
#'
#' Only display text changes. Internal value, suit, symbol, and other fields
#' remain unchanged, so comparisons and game rules are language-independent.
#' Explicit translation replaces any custom display. Unknown suit/value labels
#' are retained rather than guessed.
#' @param x A card object.
#' @param language Target language: en or es.
#' @return A card with a translated display string.
#' @export
translate_card <- function(x, language = "en") {
  if (!inherits(x, "card")) stop("Expected a card")
  if (!is.character(language) || length(language) != 1L || is.na(language) ||
      !language %in% c("en", "es")) stop("Supported languages are en and es")
  french <- x$suit %in% c("Hearts","Diamonds","Clubs","Spades","Joker")
  suits <- if (language == "en") c(Hearts="Hearts",Diamonds="Diamonds",Clubs="Clubs",
    Spades="Spades",Joker="Joker",Oros="Coins",Copas="Cups",Espadas="Swords",Bastos="Batons") else
    c(Hearts="Corazones",Diamonds="Diamantes",Clubs="Tr\u00e9boles",Spades="Picas",
      Joker="Comod\u00edn",Oros="Oros",Copas="Copas",Espadas="Espadas",Bastos="Bastos")
  ranks <- if (french) {
    if (language == "en") c(A="Ace",J="Jack",Q="Queen",K="King",Joker="Joker") else
      c(A="As",J="Jota",Q="Reina",K="Rey",Joker="Comod\u00edn")
  } else {
    if (language == "en") c("1"="Ace","10"="Jack","11"="Knight","12"="King") else
      c("1"="As","10"="Sota","11"="Caballo","12"="Rey")
  }
  value <- unname(ranks[x$value]); suit <- unname(suits[x$suit])
  if (is.na(value)) value <- x$value
  if (is.na(suit)) suit <- x$suit
  x$display <- paste(value, if (language == "en") "of" else "de", suit, x$symbol)
  x
}

#' Translate a deck display without changing card identities
#' @param x A deck object.
#' @param language Target language: en or es.
#' @return A deck with translated cards, coherent main/optional metadata, and
#' the selected language attribute. No dataset files are modified.
#' @export
translate_deck <- function(x, language = "en") {
  if (!inherits(x, "deck")) stop("Expected a deck")
  # Validate the language even for an empty deck.
  translate_card(card("A","Spades"), language)
  cards <- lapply(as_card_list(x), translate_card, language = language)
  optional <- lapply(attr(x,"optional_cards"), translate_card, language = language)
  rebuild_deck(cards, optional, language)
}
