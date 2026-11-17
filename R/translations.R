#' Translate a card display into English, Spanish, German or Italian
#'
#' Only display text changes. Internal value, suit, symbol, and other fields
#' remain unchanged, so comparisons and game rules are language-independent.
#' Explicit translation replaces any custom display. Unknown suit/value labels
#' are retained rather than guessed.
#' @param x A card object.
#' @param language Target language: en, es, de, or it.
#' @return A card with a translated display string.
#' @export
translate_card <- function(x, language = "en") {
  if (!inherits(x, "card")) stop("Expected a card")
  language <- console_language(language)
  if(language %in% names(extra_card_locales)) return(translate_extra_card(x,language))
  french <- x$suit %in% c("Hearts","Diamonds","Clubs","Spades","Joker")
  suits <- switch(language,
    en=c(Hearts="Hearts",Diamonds="Diamonds",Clubs="Clubs",Spades="Spades",Joker="Joker",Oros="Coins",Copas="Cups",Espadas="Swords",Bastos="Batons"),
    es=c(Hearts="Corazones",Diamonds="Diamantes",Clubs="Tr\u00e9boles",Spades="Picas",Joker="Comod\u00edn",Oros="Oros",Copas="Copas",Espadas="Espadas",Bastos="Bastos"),
    de=c(Hearts="Herz",Diamonds="Karo",Clubs="Kreuz",Spades="Pik",Joker="Joker",Oros="M\u00fcnzen",Copas="Kelche",Espadas="Schwerter",Bastos="St\u00e4be"),
    it=c(Hearts="Cuori",Diamonds="Quadri",Clubs="Fiori",Spades="Picche",Joker="Jolly",Oros="Denari",Copas="Coppe",Espadas="Spade",Bastos="Bastoni"))
  ranks <- if(french) switch(language,
    en=c(A="Ace",J="Jack",Q="Queen",K="King",Joker="Joker"),
    es=c(A="As",J="Jota",Q="Reina",K="Rey",Joker="Comod\u00edn"),
    de=c(A="Ass",J="Bube",Q="Dame",K="K\u00f6nig",Joker="Joker"),
    it=c(A="Asso",J="Fante",Q="Regina",K="Re",Joker="Jolly")) else switch(language,
    en=c("1"="Ace","10"="Jack","11"="Knight","12"="King"),
    es=c("1"="As","10"="Sota","11"="Caballo","12"="Rey"),
    de=c("1"="Ass","10"="Bube","11"="Reiter","12"="K\u00f6nig"),
    it=c("1"="Asso","10"="Fante","11"="Cavallo","12"="Re"))
  italian <- x$suit %in% c("Denari","Coppe","Spade","Bastoni")
  suits <- c(suits,setNames(unname(suits[c("Oros","Copas","Espadas","Bastos")]),c("Denari","Coppe","Spade","Bastoni")))
  if(italian) ranks <- switch(language,
    en=c("1"="Ace","8"="Jack","9"="Knight","10"="King"),
    es=c("1"="As","8"="Sota","9"="Caballo","10"="Rey"),
    de=c("1"="Ass","8"="Bube","9"="Reiter","10"="K\u00f6nig"),
    it=c("1"="Asso","8"="Fante","9"="Cavallo","10"="Re"))
  value <- unname(ranks[x$value]); suit <- unname(suits[x$suit])
  if (is.na(value)) value <- x$value
  if (is.na(suit)) suit <- x$suit
  x$display <- paste(value, switch(language,en="of",es="de",de="von",it="di"), suit, x$symbol)
  x
}

#' Translate a deck display without changing card identities
#' @param x A deck object.
#' @param language Target language: en, es, de, or it.
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

translate_extra_card <- function(x,language) {
  lexicon <- extra_card_locales[[language]]
  italian <- x$suit %in% c("Denari","Coppe","Spade","Bastoni")
  french <- x$suit %in% c("Hearts","Diamonds","Clubs","Spades","Joker")
  ranks <- if(italian) lexicon$italian else if(french) lexicon$french else lexicon$spanish
  value <- unname(ranks[x$value]);suit <- unname(lexicon$suits[x$suit])
  if(is.na(value)) value <- x$value
  if(is.na(suit)) suit <- x$suit
  x$display <- sprintf(lexicon$template,value,suit,x$symbol)
  x
}
