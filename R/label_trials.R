#' Label trials
#'
#' @param design Experiment Design
#' @param ... series of item = labels specifications
#'
#' @export
label_trials <- function(design, ...){
  user_labels <- list(...)

  if (any(!names(user_labels) %in% names(design[['items']])))
    stop("Name not found in add_items() specification.")

  n_labels <- length(user_labels)
  for (i in 1:n_labels) {
    item_labels <- user_labels[[i]]
    item_set <- names(user_labels[i]) # Get what kind of trial the user supplied


    # Make sure the user supplied enough trial labels
    if (length(item_labels) != design[['items']][[item_set]])
      stop("Number of provided labels must equal number of items specified in add_items()")

    attr(design[['items']][[item_set]], 'labels') <- item_labels
    attr(design[['items']][[item_set]], 'labelled') <- TRUE
  }
  design
}
