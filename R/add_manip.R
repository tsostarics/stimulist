#' Add Experimental Manipulations
#'
#' @param .data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_manip <- function(.data, ...){
  .data[['manipulations']] <-  list(...)
  .data
}
