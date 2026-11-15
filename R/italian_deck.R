#' Construct a forty-card Italian deck
#'
#' Canonical suits are Denari, Coppe, Spade and Bastoni. Ranks 8, 9 and 10
#' represent fante, cavallo and re. This constructor writes no dataset files.
#' @param language Display language: en, es, de or it.
#' @return A deck with forty distinct cards and no optional cards.
#' @export
italian_deck <- function(language = "it") {
  language <- console_language(language)
  cards <- unlist(lapply(c("Denari","Coppe","Spade","Bastoni"),function(suit) {
    lapply(as.character(1:10),function(value) translate_card(card(value,suit),language))
  }),recursive=FALSE)
  deck(cards=cards,language=language)
}

italian_card_ids <- function(cards) {
  if(!is.list(cards) || !all(vapply(cards,function(x) inherits(x,"card") &&
     x$value %in% as.character(1:10) && x$suit %in% c("Denari","Coppe","Spade","Bastoni"),logical(1))))
    stop("Expected standard Italian cards")
  ids <- vapply(cards,function(x) paste(x$value,x$suit),"")
  if(anyDuplicated(ids)) stop("Duplicate Italian cards")
  ids
}
