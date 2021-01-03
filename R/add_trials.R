#' Add trials to experiment design
#'
#' @param .data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_trials <- function(.data, ...){
  .data[['trials']] <- list(...)
  attributes(.data[['trials']])$total <- sum(unlist(.data[['trials']]))
  .data
}
