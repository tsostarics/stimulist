#' Add Experimental Manipulations
#'
#' @param design
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_manipulations <- function(design, ...){
  design[['manipulations']] <-  list(...)
  for (i in 1:length(design[['manipulations']])) {
    attr(design[['manipulations']][[i]], 'ordering') <- FALSE
  }
  design
}
