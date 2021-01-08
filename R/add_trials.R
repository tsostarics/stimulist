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
  attr(design[['trials']], 'total') <- sum(unlist(design[['trials']]))
  for (i in 1:length(design[['trials']])){
    attr(design[['trials']][[i]], 'labels') <- ''
    attr(design[['trials']][[i]], 'labelled') <- FALSE
  }
  design
}
