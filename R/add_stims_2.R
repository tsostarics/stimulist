#' Title
#'
#' @param design
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_stims_2 <- function(design, ...){
  stims <- ensyms(...)
  trial_types <- lmap(design[['items']],
                      function(x) setNames(list(1:x[[1L]]), names(x)))
  stimsets <- lmap(stims,
                   function(x) setNames(list(trial_types), as.character(x)))
  previous_stims <- length(design[['stimuli']])

  if (previous_stims != 0) design[['stimuli']][[previous_stims + 1L]] <- stimsets
  else design[['stimuli']] <- stimsets

  design
}
