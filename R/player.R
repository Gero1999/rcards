#' Player constructor
#'
#' @param hand List of cards or deck object (default: empty list)
#' @param role Player role (default: "standard")
#' @param points Player points (default: 0)
#' @return An object of class 'player'
player <- function(name = "Bob", hand = list(), role = "standard", points = 0) {
	structure(
		list(
            name = name,
			hand = hand,
			role = role,
			points = points
		),
		class = "player"
	)
}

#' Print method for player
print.player <- function(x, ...) {
	cat("Player (role:", x$role, ", points:", x$points, ", hand size:", length(x$hand), ")\n")
	invisible(x)
}
