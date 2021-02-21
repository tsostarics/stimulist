#' Title
#'
#' @param x object
#' @param ... additional arguments
#'
#' @return
#' @export
#'
#' @examples
print.stimulist <- function(x, ...) {
  cat(attr(x[['name']], "printmsg"))
  cat(attr(x[['manipulations']], "printmsg"))
  cat(attr(x[['items']], "printmsg"))
  cat(attr(x[['stimuli']], "printmsg"))
  cat(attr(x[['counterbalance']], "printmsg"))
  cat(attr(x[['complete_experiment']], "printmsg"))
}
