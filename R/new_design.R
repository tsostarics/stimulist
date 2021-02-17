#' Create new design
#'
#' @return
#' @export
#'
#' @examples
new_design <- function(name = "Experiment"){
  design <-
    list(
      manipulations = list(),
      items = list(),
      stimuli = list(),
      orderings = list(),
      presentations = list(),
      counterbalance = NA,
      complete_experiment = NA,
      name = name
    )
  attr(design$manipulations, 'printmsg') <- 'No manipulations set yet.\n'
  attr(design$items, 'printmsg') <- 'No items set yet.\n'
  attr(design$stimuli, 'printmsg') <- 'No stimuli set yet.\n'
  attr(design$orderings, 'printmsg') <- 'No orderings set yet.\n'
  attr(design$presentations, 'printmsg') <- 'No presentations set yet.\n'
  attr(design$counterbalance, 'printmsg') <- 'No counterbalancing set.\n'
  attr(design$name, 'printmsg') <- paste0('Experiment Name: ', name, "\n")
  attr(design$items, 'total') <- 0
  attr(design$stimuli, 'placeholder') <- TRUE
  attr(design$counterbalance, 'printmsg') <- ''
  class(design) <- 'stimulist'
  design
}
