#' Label trials
#'
#' @param design Experiment Design
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
label_trials <- function(design, ...){
  user_labels <- enexprs(...)
  n_labels <- length(user_labels)
  for (i in 1:n_labels) {
    label <- user_labels[i]
    trial_set <- names(label) # Get what kind of trial the user supplied

    # Make sure the user supplied an existing type of trial
    assert_that(has_name(design[['trials']], trial_set))

    # Make sure the user supplied enough trial labels
    trial_labels <- eval(label[[1L]])
    if (length(trial_labels) != design[['trials']][[trial_set]])
      stop("Number of provided labels must equal number of trials specified")

    attr(design[['trials']][[trial_set]], 'labels') <- trial_labels
    attr(design[['trials']][[trial_set]], 'labelled') <- TRUE
  }
  design
}
