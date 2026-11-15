#' Configure Blackjack rules
#' @param decks Number of French decks, one through eight; default six.
#' @param hit_soft17 Whether the dealer hits soft seventeen.
#' @param blackjack_payout Profit multiplier for a natural: 1.5 (3:2) or 1.2 (6:5).
#' @param double_after_split Allow doubling after non-ace splits.
#' @param max_hands Maximum hands per player after splitting, one through four.
#' @return A validated Blackjack rules object. Split aces always draw once and stand.
#' @export
blackjack_rules <- function(decks=6L,hit_soft17=FALSE,blackjack_payout=1.5,
                            double_after_split=TRUE,max_hands=4L) {
  check_number(decks,"decks",1,8,TRUE)
  check_number(max_hands,"max_hands",1,4,TRUE)
  for(flag in list(hit_soft17,double_after_split))
    if(!is.logical(flag) || length(flag)!=1L || is.na(flag)) stop("Rule flags must be TRUE or FALSE")
  if(!is.numeric(blackjack_payout) || length(blackjack_payout)!=1L || is.na(blackjack_payout) ||
     !blackjack_payout %in% c(1.5,1.2)) stop("Natural payout must be 1.5 or 1.2")
  structure(list(decks=decks,hit_soft17=hit_soft17,blackjack_payout=blackjack_payout,
    double_after_split=double_after_split,max_hands=max_hands),class="blackjack_rules")
}
