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

locale_catalog <- list()
