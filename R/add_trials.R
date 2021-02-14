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
  for (i in 1:length(design[['trials']])) {
    attr(design[['trials']][[i]], 'labels') <- ''
    attr(design[['trials']][[i]], 'labelled') <- FALSE
  }
  .set_trials_printmsg(design)
}

.set_trials_printmsg <- function(design){
  new_printmsg <- paste0(attr(design$trials, 'total')," trials total:\n")

  for (i in 1:length(design[['trials']])) {
    new_printmsg <-
      paste0(new_printmsg,
             paste0("  ",
                    names(design[['trials']][i]),
                    ": ",
                    design[['trials']][[i]],
                    "\n")
      )
  }

  attr(design$trials, 'printmsg') <- new_printmsg
  design
}
