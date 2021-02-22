#' Fill out experiment
#'
#' After setting all the components of your experiment, generates a full table
#' of all the stimuli and saves it to the complete_experiment entry of the
#' design. The output of this function can then be passed to save_lists() to
#' save separate counterbalanced stimulus lists to use.
#'
#' @param design Experiment design
#' @param use_as_is Whether to use all the generated combinations as trials. Use
#' when you plan to present all of the trials to every participant.
#'
#' @export
fill_experiment <- function(design, use_as_is = F) {
  stim_table <- get_stim_table(design)
  expanded <-
    merge(
      stim_table,
      Reduce(
        function(x, y) {
          merge(x, y, all = F)
        },
        design[["presentations"]]
      ),
      all = F,
      allow.cartesian = T
    )
  expanded[['tojoin']] <- NULL
  is_counterbalanced <- any(!is.na(design[["counterbalance"]]))
  # If counterbalance is specified, add to the expanded table
  if (is_counterbalanced) {
    expanded$counterbalance <- design[["counterbalance"]]
  }

  if (use_as_is) {
    expanded$trial <- 1:nrow(expanded)
    attr(design$items, "printmsg") <-
      paste0(nrow(expanded), " items, participants will be given the entire set of stimuli.\n")
    if (is_counterbalanced) {
      warning("You've specified counterbalancing by participant, yet you've set `use_as_is` as TRUE, suggesting a within-subjects design.")
    }
  }
  design[["complete_experiment"]] <- expanded
  .set_fill_printmsg(design, is_counterbalanced)
}

.set_fill_printmsg <- function(design, is_counterbalanced) {
  n_trials <- nrow(design[["complete_experiment"]])
  new_printmsg <- paste0("Completed table of ", n_trials, " trials ")
  if (is_counterbalanced) {
    new_printmsg <- paste0(
      new_printmsg,
      "(",
      n_trials / max(design[["complete_experiment"]][["counterbalance"]]),
      " per list) is ready.\n"
    )
  } else {
    new_printmsg <- paste0(new_printmsg, "is ready.\n")
  }
  attr(design[["complete_experiment"]], "printmsg") <- new_printmsg
  design
}
