#' Probability of at least one successful draw without replacement
#' @param successes Number of successful cards among the remaining cards.
#' @param remaining Total remaining cards.
#' @param draws Number of cards drawn without replacement.
#' @return An exact probability under uniform random draws. This is a draw
#' probability, not a probability of winning a game.
#' @export
draw_probability <- function(successes,remaining,draws=1L) {
  check_number(remaining,"remaining",0,integer=TRUE)
  check_number(successes,"successes",0,remaining,TRUE)
  check_number(draws,"draws",0,remaining,TRUE)
  if(draws==0 || successes==0) return(0)
  if(draws>remaining-successes) return(1)
  -expm1(lchoose(remaining-successes,draws)-lchoose(remaining,draws))
}

estimate_n <- function(n,maximum=100000L) {
  check_number(n,"n",1,maximum,TRUE)
  as.integer(n)
}

# Wilson intervals for Bernoulli events; normal Monte Carlo intervals for means.
# No simulation variance is claimed for exact enumeration or one-sample means.
estimate_summary <- function(values,binary=FALSE,exact=FALSE,level=0.95,bounds=NULL) {
  check_number(level,"level",0,1)
  if(level==0 || level==1) stop("level must be strictly between zero and one")
  values <- values[!is.na(values)];n <- length(values)
  if(!n) return(data.frame(estimate=NA_real_,standard_error=NA_real_,lower=NA_real_,upper=NA_real_,samples=0L))
  if(any(!is.finite(values))) stop("Nonfinite statistical sample")
  mean <- mean(values)
  z <- stats::qnorm((1+level)/2)
  if(exact) {se<-0;interval<-c(mean,mean)} else if(binary) {
    if(!all(values %in% c(0,1))) stop("Binary samples must be zero or one")
    se <- sqrt(mean*(1-mean)/n)
    center <- (mean+z^2/(2*n))/(1+z^2/n)
    half <- z*sqrt(mean*(1-mean)/n+z^2/(4*n^2))/(1+z^2/n)
    interval <- c(center-half,center+half)
  } else if(n>1L) {
    se <- stats::sd(values)/sqrt(n);interval <- mean+c(-1,1)*z*se
  } else {se<-NA_real_;interval<-c(NA_real_,NA_real_)}
  if(!is.null(bounds)) interval <- pmax(bounds[1],pmin(bounds[2],interval))
  data.frame(estimate=mean,standard_error=se,lower=interval[1],upper=interval[2],samples=n)
}

stat_report <- function(type,method,assumptions,estimates,details=list()) {
  structure(c(list(type=type,method=method,assumptions=assumptions,
    confidence_level=0.95,estimates=estimates),details),class="rcards_estimate")
}

#' Estimate hand strength from a public game observation
#' @param observation A result of game_observation(), not full engine state.
#' @param ... Game-specific estimation options.
#' @return A report with method, assumptions, estimates, sample counts and
#' Monte Carlo uncertainty. Supported games are added in game-specific modules.
#' @export
estimate_hand <- function(observation,...) {
  if(!is.list(observation) || !is.character(observation$game_type) ||
     length(observation$game_type)!=1L || is.na(observation$game_type)) stop("Provide a public game observation")
  switch(observation$game_type,
    stop("Hand estimation is not available for this game"))
}
