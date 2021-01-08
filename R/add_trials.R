#' Add trials to experiment design
#'
#' @param design
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_trials <- function(design, ...){
  design[['trials']] <- list(...)
  attributes(design[['trials']])$total <- sum(unlist(design[['trials']]))
  design
}
