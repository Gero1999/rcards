new_holdem <- function(stacks = c(100,100), ...) {
  holdem_hand(lapply(seq_along(stacks), function(i) player(paste0("P", i), points = stacks[i])), ...)
}
finish_calling <- function(h) {
  limit <- 0L
  while (!h$complete) {
    legal <- holdem_legal_actions(h)
    h <- holdem_action(h, if ("check" %in% legal$actions) "check" else "call")
    limit <- limit + 1L
    if (limit > 200) stop("Hand failed to terminate")
  }
  h
}

