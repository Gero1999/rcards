#' Select Mus rules
#'
#' Four clockwise seats form partnerships 1/3 and 2/4. In eight-kings play,
#' threes count as kings and twos as aces in every lance. No signals, wildcards,
#' or house-specific real-31 exceptions are implemented. Ties go to mano order.
#' @param kings Either 4 or 8 (default 8).
#' @param points_to_win Game target, 30 or 40 (default 40).
#' @return A mus_rules configuration for mus_game().
#' @export
mus_rules <- function(kings = 8L, points_to_win = 40L) {
  check_number(kings,"kings",4,8,TRUE)
  if (!kings %in% c(4L,8L)) stop("kings must be 4 or 8")
  check_number(points_to_win,"points_to_win",30,40,TRUE)
  if (!points_to_win %in% c(30L,40L)) stop("points_to_win must be 30 or 40")
  structure(list(kings=kings,points_to_win=points_to_win),class="mus_rules")
}

mus_pack <- function() {
  cards <- unlist(lapply(c("Oros","Copas","Espadas","Bastos"),function(suit) {
    lapply(c(as.character(1:7),"10","11","12"),card,suit=suit)
  }),recursive=FALSE)
  deck(cards=cards,language="es")
}

mus_card_ids <- function(cards) {
  if (!is.list(cards)) stop("Expected a card list")
  ids <- vapply(cards,function(x) {
    if (!inherits(x,"card") || !x$value %in% c(as.character(1:7),"10","11","12") ||
        !x$suit %in% c("Oros","Copas","Espadas","Bastos")) stop("Expected a 40-card Spanish pack")
    paste(x$value,x$suit,sep=":")
  },character(1))
  if(anyDuplicated(ids)) stop("Duplicate Spanish cards")
  ids
}

#' Start a four-player partnership Mus game
#'
#' Starts with a mus/cut vote and four cards per player. Unanimous mus allows
#' each player to discard once, then another vote. Any cut starts grande, chica,
#' pares, and juego (or punto when nobody has juego). Use mus_next_hand() after
#' a hand finishes, until one partnership reaches the configured game target.
#' @param players Four player objects or four distinct player names.
#' @param rules Configuration from mus_rules().
#' @param mano First seat to act; ties favor clockwise order starting here.
#' @param seed Optional reproducible shuffle seed, preserving caller RNG.
#' @param pack Optional ordered complete 40-card Spanish deck.
#' @return A mus_game state with scores for partnerships 1/3 and 2/4.
#' @export
mus_game <- function(players, rules = mus_rules(), mano = 1L, seed = NULL, pack = NULL) {
  if (is.character(players)) players <- lapply(players,player)
  if (!is.list(players) || length(players)!=4L ||
      !all(vapply(players,inherits,logical(1),"player"))) stop("Expected four players")
  names <- vapply(players,`[[`,"","name")
  if (anyNA(names) || any(!nzchar(names)) || anyDuplicated(names)) stop("Player names must be distinct")
  if(!inherits(rules,"mus_rules")) stop("Expected mus_rules")
  rules <- mus_rules(rules$kings,rules$points_to_win)
  check_number(mano,"mano",1,4,TRUE)
  if(is.null(pack)) pack <- with_poker_seed(seed,function() shuffle(mus_pack()))
  if(length(mus_card_ids(pack))!=40L) stop("Expected a complete 40-card Spanish pack")
  g <- structure(list(players=players,rules=rules,mano=mano,actor=mano,
    deck=deck(cards=as_card_list(pack),language="es"),discard=list(),phase="mus",
    scores=c(0,0),complete=FALSE,hand_complete=FALSE,hand_number=1L,
    votes=integer(),exchanged=integer(),history=list(),results=list()),class="mus_game")
  for(i in 1:4) g$players[[i]]$hand <- list()
  order <- mus_order(mano)
  for(round in 1:4) for(i in order) {
    g$players[[i]]$hand <- c(g$players[[i]]$hand,as_card_list(g$deck[1]))
    g$deck <- g$deck[-1L]
  }
  g
}

mus_order <- function(mano) ((0:3+mano-1L) %% 4L)+1L
mus_team <- function(seat) ifelse(seat %% 2L == 1L,1L,2L)

mus_values <- function(hand,rules) {
  if(length(hand)!=4L) stop("Mus hands need four cards")
  mus_card_ids(hand)
  ranks <- as.integer(vapply(hand,`[[`,"","value"))
  if(rules$kings==8L) {
    ranks[ranks==3L] <- 12L; ranks[ranks==2L] <- 1L
  }
  ranks
}

mus_pair_score <- function(ranks) {
  counts <- base::table(ranks)
  quads <- as.integer(names(counts))[counts==4L]
  triples <- as.integer(names(counts))[counts==3L]
  pairs <- sort(as.integer(names(counts))[counts==2L],decreasing=TRUE)
  if(length(quads)) return(c(3L,quads,quads))
  if(length(triples)) return(c(2L,triples,0L))
  if(length(pairs)==2L) return(c(3L,pairs))
  if(length(pairs)) return(c(1L,pairs,0L))
  c(0L,0L,0L)
}

mus_hand_info <- function(hand,rules) {
  ranks <- mus_values(hand,rules)
  pair <- mus_pair_score(ranks)
  total <- sum(pmin(ranks,10L))
  # Legal 40-card packs cannot produce totals 38 or 39.
  juego <- match(total,c(33L,34L,35L,36L,37L,40L,32L,31L),nomatch=0L)
  list(grande=sum(sort(ranks,decreasing=TRUE)*13^(3:0)),
    chica=sum((13L-sort(ranks))*13^(3:0)),
    pares=sum(pair*13^(2:0)),pairs_points=pair[1L],
    juego=juego,juego_points=if(total==31L) 3L else if(total>=31L) 2L else 0L,
    punto=total)
}

mus_infos <- function(g) lapply(g$players,function(p) mus_hand_info(p$hand,g$rules))

mus_begin_lance <- function(g,stage) {
  repeat {
    if(stage>4L) {
      g$phase <- "hand_complete";g$hand_complete <- TRUE;g$actor <- NULL
      return(g)
    }
    g$stage <- stage;g$phase <- c("grande","chica","pares","juego")[stage]
    info <- mus_infos(g)
    eligible <- 1:4
    if(stage==3L) eligible <- which(vapply(info,`[[`,0,"pairs_points")>0)
    if(stage==4L) {
      eligible <- which(vapply(info,`[[`,0,"juego_points")>0)
      if(!length(eligible)) {g$phase <- "punto";eligible <- 1:4}
    }
    g$eligible <- eligible
    if(!length(eligible)) {stage <- stage+1L;next}
    if(length(unique(mus_team(eligible)))==1L) {
      team <- mus_team(eligible[1])
      points <- mus_intrinsic(g,team)
      g <- mus_award(g,team,points,"unopposed")
      if(g$complete) return(g)
      stage <- stage+1L;next
    }
    g$wager <- 0;g$accepted <- 0;g$bet_team <- NULL;g$ordago <- FALSE;g$passed <- integer()
    g$actor <- mus_order(g$mano)[mus_order(g$mano) %in% eligible][1L]
    return(g)
  }
}

mus_intrinsic <- function(g,team) {
  info <- mus_infos(g)
  seats <- which(mus_team(1:4)==team)
  if(g$phase=="pares") return(sum(vapply(info[seats],`[[`,0,"pairs_points")))
  if(g$phase=="juego") return(sum(vapply(info[seats],`[[`,0,"juego_points")))
  if(g$phase=="punto") return(1L)
  0L
}

mus_winner <- function(g) {
  info <- mus_infos(g)
  order <- mus_order(g$mano);order <- order[order %in% g$eligible]
  scores <- vapply(info[order],`[[`,0,g$phase)
  mus_team(order[which.max(scores)])
}

mus_award <- function(g,team,points,reason) {
  g$scores[team] <- g$scores[team]+points
  g$results[[length(g$results)+1L]] <- list(hand=g$hand_number,lance=g$phase,
    team=team,points=points,reason=reason)
  if(g$scores[team]>=g$rules$points_to_win) {
    g$complete <- TRUE;g$hand_complete <- TRUE;g$winner <- team
    g$phase <- "complete";g$actor <- NULL
  }
  g
}

mus_resolve <- function(g,team,points,reason) {
  g <- mus_award(g,team,points,reason)
  if(g$complete) g else mus_begin_lance(g,g$stage+1L)
}

#' Inspect legal Mus actions
#' @param game An active mus_game state.
#' @return Actor, phase, and available actions. Bets/raises use total stake amounts.
#' @export
mus_legal_actions <- function(game) {
  if(!inherits(game,"mus_game")) stop("Expected mus_game")
  if(game$hand_complete) return(list(actor=NULL,phase=game$phase,actions=character()))
  actions <- if(game$phase=="mus") c("mus","cut") else if(game$phase=="exchange") "discard" else
    if(is.null(game$bet_team)) c("pass","bet","ordago") else
      if(game$ordago) c("accept","reject") else c("accept","reject","raise","ordago")
  list(actor=game$actor,phase=game$phase,actions=actions)
}

#' Apply a Mus vote, exchange, or wager
#'
#' A bet starts at two points; a raise increases the total by at least two.
#' Acceptance resolves the lance. Rejecting awards the previous offered stake
#' (one point for an initial refusal), plus winning partnership's intrinsic
#' pares/juego points. An accepted ordago immediately decides the game by the
#' current lance; rejection behaves like refusal of the preceding offer.
#' All-pass grande/chica/punto award one; all-pass pares/juego award intrinsic
#' points only. Grande, chica, pares and juego are scored in that order.
#' @param game A mus_game state.
#' @param action One of the actions returned by mus_legal_actions().
#' @param amount Total stake for bet/raise; NULL for other actions.
#' @param cards Unique hand positions for discard; integer(0) stands pat.
#' @param seed Optional RNG seed when a depleted stock requires shuffling discards.
#' @return Updated game state. Inputs are not modified.
#' @export
mus_action <- function(game,action,amount=NULL,cards=integer(),seed=NULL) {
  legal <- mus_legal_actions(game)
  if(length(action)!=1L || is.na(action) || !action %in% legal$actions) stop("Illegal Mus action")
  if(!action %in% c("bet","raise") && !is.null(amount)) stop("Only bets/raises take amounts")
  if(action!="discard" && length(cards)) stop("Only discards take card positions")
  g <- game;i <- g$actor;team <- mus_team(i)
  g$history[[length(g$history)+1L]] <- list(hand=g$hand_number,phase=g$phase,
    player=i,action=action,amount=amount,cards=cards)
  if(g$phase=="mus") {
    if(action=="cut") return(mus_begin_lance(g,1L))
    g$votes <- c(g$votes,i)
    if(length(g$votes)==4L) {g$phase <- "exchange";g$actor <- g$mano;g$exchanged <- integer()} else
      g$actor <- mus_order(i %% 4L+1L)[1L]
    return(g)
  }
  if(g$phase=="exchange") {
    hand <- g$players[[i]]$hand
    validate_positions(cards,4L)
    n <- length(cards)
    # Keep this player's current discards out of the replacement stock.
    if(length(g$deck)<n) {
      available <- c(as_card_list(g$deck),g$discard)
      if(length(available)<n) stop("Insufficient replacement stock")
      g$deck <- with_poker_seed(seed,function() shuffle(deck(cards=available,language="es")))
      g$discard <- list()
    }
    g$players[[i]]$hand <- c(hand[setdiff(1:4,cards)],as_card_list(g$deck)[seq_len(n)])
    g$deck <- g$deck[setdiff(seq_along(g$deck),seq_len(n))]
    g$discard <- c(g$discard,hand[cards])
    g$exchanged <- c(g$exchanged,i)
    if(length(g$exchanged)==4L) {g$phase <- "mus";g$votes <- integer();g$actor <- g$mano} else
      g$actor <- (i %% 4L)+1L
    return(g)
  }
  if(action=="pass") {
    g$passed <- c(g$passed,i)
    if(length(g$passed)==length(g$eligible)) {
      winner <- mus_winner(g)
      points <- mus_intrinsic(g,winner)
      if(g$phase %in% c("grande","chica")) points <- 1L
      return(mus_resolve(g,winner,points,"passed"))
    }
    order <- mus_order((i %% 4L)+1L)
    g$actor <- order[order %in% g$eligible & !order %in% g$passed][1L]
    return(g)
  }
  if(action=="accept") {
    winner <- mus_winner(g)
    if(g$ordago) {
      return(mus_award(g,winner,g$rules$points_to_win-g$scores[winner],"ordago"))
    }
    points <- g$wager + if(g$phase=="punto") 0L else mus_intrinsic(g,winner)
    return(mus_resolve(g,winner,points,"accepted"))
  }
  if(action=="reject") {
    points <- max(1L,g$accepted)
    # Intrinsic pairs/juego belong to the partnership that wins the refused lance.
    if(g$phase %in% c("pares","juego")) points <- points+mus_intrinsic(g,g$bet_team)
    return(mus_resolve(g,g$bet_team,points,"refused"))
  }
  if(action %in% c("bet","raise")) {
    minimum <- if(action=="bet") 2L else g$wager+2L
    check_number(amount,"stake",minimum,integer=TRUE)
  }
  g$accepted <- g$wager
  if(action=="ordago") g$ordago <- TRUE else g$wager <- amount
  g$bet_team <- team
  order <- mus_order((i %% 4L)+1L)
  g$actor <- order[order %in% g$eligible & mus_team(order)!=team][1L]
  g
}

#' Deal another Mus hand
#' @param game A completed hand in a mus_game that has not reached its target.
#' @param seed Optional seed for the new shuffle.
#' @return A new hand preserving partnership scores and rotating mano clockwise.
#' @export
mus_next_hand <- function(game,seed=NULL) {
  if(!inherits(game,"mus_game") || !game$hand_complete || game$complete) stop("Finish the current hand; game must not be complete")
  next_game <- mus_game(game$players,game$rules,game$mano %% 4L+1L,seed)
  next_game$scores <- game$scores;next_game$hand_number <- game$hand_number+1L
  next_game$history <- game$history;next_game$results <- game$results
  next_game
}
