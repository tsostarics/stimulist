#' Fill out experiment
#'
#' After setting all the components of your experiment, generates a full table
#' of all the stimuli and saves it to the complete_experiment entry of the
#' design. The output of this function can then be passed to save_lists() to
#' save separate counterbalanced stimulus lists to use.
#'
#' @param design
#'
#' @return
#' @export
#'
#' @examples
fill_experiment <- function(design){
  stim_table <- get_stim_table(design)
  expanded <-
    merge(
      stim_table,
      Reduce(function(x,y)
        merge(x,y, all = F),
        design[['presentations']]
      ),
      # by = 'tojoin',
      all = F,
      allow.cartesian = T
    )
  expanded$tojoin <- NULL
  if (any(!is.na(design[['counterbalance']])))
    expanded$counterbalance <- design[['counterbalance']]

  design[['complete_experiment']] <- expanded
  design
}
