##' Game constructor
##'
##' @param players List of player objects (default: empty list)
##' @param table Table object (optional, default: NULL)
##' @param decks List of deck objects (default: empty list)
##' @return An object of class 'game'
game <- function(players = list(), table = NULL, decks = list()) {
	structure(
		list(
			players = players,
			table = table,
			decks = decks
		),
		class = "game"
	)
}

##' Print method for game
print.game <- function(x, ...) {
	n_players <- length(x$players)
	n_decks <- length(x$decks)
	has_table <- !is.null(x$table)
	cat("Game (", n_players, " player(s), ", n_decks, " deck(s)", if (has_table) ", table present" else ", no table", ")\n", sep = "")
	invisible(x)
}
