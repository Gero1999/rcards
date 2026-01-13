##' Table constructor
##'
##' @param decks List of deck objects (default: empty list)
##' @param points Numeric value for table points (default: 0)
##' @return An object of class 'table'
table <- function(decks = list(), points = 0) {
	structure(
		list(
			decks = decks,
			points = points
		),
		class = "table"
	)
}

##' Print method for table
print.table <- function(x, ...) {
	n_decks <- length(x$decks)
	cat("Table (", n_decks, " deck(s), points: ", x$points, ")\n", sep = "")
	invisible(x)
}
