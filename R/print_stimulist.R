#' Title
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
print.stimulist <- function(obj){
  cat(attr(obj$name, 'printmsg'))
  cat(attr(obj$manipulations, 'printmsg'))
  cat(attr(obj$items, 'printmsg'))
  cat(attr(obj$stimuli, 'printmsg'))
  cat(attr(obj$counterbalance, "printmsg"))
  cat(attr(obj$complete_experiment, 'printmsg'))
}
