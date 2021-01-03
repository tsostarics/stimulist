#' Create new design
#'
#' @return
#' @export
#'
#' @examples
new_design <- function(){
  design <-
    list(
      manipulations = list(),
      trials = list(),
      stimuli = list(),
      counterbalance = NA
    )
  attr(design$trials, 'total') <- 0
  attr(design$stimuli, 'placeholder') <- TRUE
  design
}
