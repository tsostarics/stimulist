#' Split lists by
#'
#' @param design Experiment design
#' @param ... a series of unquoted manipulation or variable that you
#' want to split the lists by. For example, if you have the same set up but in
#' 3 different languages, you may opt to split the lists by language. Right now
#' it will overwrite any counterbalancing that's set in the output files.
#'
#' @export
split_lists_by <- function(design, ...) {
  attr(design[["counterbalance"]], "splits") <- vapply(ensyms(...), as.character, "char")
  design
}
