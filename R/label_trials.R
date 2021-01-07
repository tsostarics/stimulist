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
  # ensure that lhs matches a type of trial (eg critical, filler)
  # ensure that rhs has the same length as the number of trials specified
  # eg: label_trials(critical = c('...'))) where length of the vector ==
  #  design[['trials']][['critical']]
  for (i in 1:n_labels) {
    label <- user_labels
    trial_set <- names(label)
    trial_labels <- eval(label[[1L]])

    if (length(trial_labels) != design[['trials']][[trial_set]])
      stop("Number of provided labels must equal number of trials specified")
    attr(design[['trials']][[trial_set]], 'labels') <- trial_labels
  }
  design
}
