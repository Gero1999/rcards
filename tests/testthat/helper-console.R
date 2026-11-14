console_fixture <- function(lines) {
  index <- 0L; output <- character(); prompts <- character()
  reader <- function(prompt) {
    prompts <<- c(prompts, prompt)
    index <<- index + 1L
    if (index > length(lines)) return(character())
    value <- lines[[index]]
    if (inherits(value, "condition")) stop(value)
    value
  }
  list(controller = console_player(read = reader, write = function(text) output <<- c(output, text)),
    output = function() paste(output, collapse = "\n"), prompts = function() prompts)
}

passive_controller <- function(o) list(action = if ("check" %in% o$legal$actions) "check" else "call")
passive_mus_controller <- function(o) list(action = if ("cut" %in% o$legal$actions) "cut" else "pass")

