#' Supported console languages
#' @return Language codes en, es, de, and it. English is the fallback.
#' @export
supported_languages <- function() c("en", "es", "de", "it")

console_language <- function(language = NULL) {
  if(is.null(language)) return("en")
  if(!is.character(language) || length(language)!=1L || is.na(language) ||
     !language %in% supported_languages()) stop("Supported languages are en, es, de, it")
  language
}

# Catalog entries use English templates as stable keys. Interpolation happens
# after lookup, so player names, numbers and controller data stay unchanged.
ui <- function(key, language = NULL, ...) {
  language <- console_language(language)
  entry <- locale_catalog[[key]]
  text <- if(language=="en" || is.null(entry)) key else unname(entry[language])
  if(is.na(text) || !nzchar(text)) text <- key
  if(length(list(...))) sprintf(text, ...) else text
}


console_action <- function(token, legal, language = NULL) {
  if(token %in% c(legal,"quit","q","exit")) return(if(token %in% c("q","exit")) "quit" else token)
  language <- console_language(language)
  aliases <- action_aliases[[language]]
  candidates <- names(aliases)[vapply(aliases,function(words) token %in% words,logical(1))]
  candidates <- intersect(candidates,c(legal,"quit"))
  if(length(candidates)==1L) candidates else token
}

action_aliases <- list(
  en=list(),
  es=list(claim="declarar",challenge="mentiroso",fold="retirarse",check="pasar",call="igualar",raise="subir",quit="salir",
    cut="cortar",discard="descartar",bet="apostar",accept="aceptar",reject="rechazar",pass="pasar",
    hit="pedir",stand="plantarse",double="doblar",split="dividir",play="jugar"),
  de=list(claim="ansagen",challenge="anzweifeln",fold="aussteigen",check="schieben",call="mitgehen",raise="erh\u00f6hen",quit="beenden",
    cut="beendenmus",discard="tauschen",bet="setzen",accept="annehmen",reject="ablehnen",pass="passen",
    hit="ziehen",stand="halten",double="verdoppeln",split="teilen",play="spielen"),
  it=list(claim="dichiarare",challenge="bugiardo",fold="lasciare",check="passare",call="vedere",raise="rilanciare",quit="uscire",
    cut="tagliare",discard="scartare",bet="puntare",accept="accettare",reject="rifiutare",pass="passare",
    hit="carta",stand="stare",double="raddoppiare",split="dividere",play="giocare")
)

action_label <- function(action, language = NULL) {
  aliases <- action_aliases[[console_language(language)]][[action]]
  if(length(aliases)) aliases[1L] else action
}

console_error <- function(message, language = NULL) {
  pattern <- "^(.+) must be a finite (integer|number) between (.+) and (.+)$"
  match <- regmatches(message,regexec(pattern,message))[[1L]]
  if(length(match)) return(ui("%s must be a finite %s between %s and %s",language,
    ui(match[2],language),ui(match[3],language),match[4],match[5]))
  ui(message,language)
}
