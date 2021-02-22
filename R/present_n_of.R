#' Present n of the conditions of a manipulation
#'
#' @param design Experimental design
#' @param manipulation Manipulation, can be quoted or unquoted
#' @param n Number to present
#'
#' @export
#' @importFrom gtools permutations
present_n_of <- function(design, manipulation, n) {
  # Enable unquoted argument for ease
  manipulation <- as.character(enexpr(manipulation))

  if (!manipulation %in% names(design[["manipulations"]])) {
    stop(paste0("manipulation ", manipulation, " not found in current design"))
  }
  if (n > length(design[["manipulations"]][[manipulation]])) {
    stop("n is greater than the number of conditions for this manipulation")
  }

  # Get the conditions for the manipulation
  conditions <- design[["manipulations"]][[manipulation]]

  # Get a matrix with each permutation of "condition pick n"
  perm_matrix <- data.frame(permutations(length(conditions), r = n, conditions))

  # Set names of each column in the matrix to manipulation_1...n
  choice_names <- vapply(1:n,
    FUN = function(x) paste(manipulation, x, sep = "_"),
    FUN.VALUE = "character"
  )
  names(perm_matrix) <- choice_names

  # Retain the original info of the manipulation, but set the new ordering
  design[["orderings"]][[manipulation]] <- perm_matrix
  attr(design[["manipulations"]][[manipulation]], "has_order") <- TRUE
  attr(design[["orderings"]][[manipulation]], "r") <- n
  attr(design[["orderings"]][[manipulation]], "n") <- length(design[["manipulations"]][[manipulation]])
  design
}
