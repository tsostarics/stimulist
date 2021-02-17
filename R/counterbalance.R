#' Counterbalance experiment
#'
#' This will counterbalance the entire experiment using a latin square approach
#' by default. Right now it's actually the only method, but in the future this
#' can be expanded further. Future functionality will also allow for different
#' types of stimuli to be counterbalanced separately (eg, critical items vs
#' filler items).
#'
#' @param design
#' @param method
#'
#' @return
#' @export
#'
#' @examples
counterbalance <- function(design, method = "latinsquare"){
  design[['counterbalance']] <- .assign_latinsquare(design)
  .set_counterbalance_printmsg(design)
}

# Get the number of lists we need to make while preventing double counting
# when incorporating crossed manipulations
.calculate_lists <- function(design){
  presentations <- names(design[['presentations']])
  any_crossed <- grepl(' x ', presentations)
  if (any(any_crossed)) {
    crossed_presentations <- presentations[any_crossed]
    manips_to_check <- c(str_split(crossed_presentations, " x ")[[1]],
                         'constant_for_all')
    reduced_presentations <- presentations[!presentations %in% manips_to_check]
  }
  else
    reduced_presentations <- presentations

  prod(vapply(design[['presentations']][reduced_presentations], nrow, 1L))
}

.set_counterbalance_printmsg <- function(design){
  n_lists <- max(design[['counterbalance']])
  new_printmsg <- paste0(n_lists," stimulus lists total, counterbalanced by:\n")
  for (i in 1:length(design$manipulations)) {
    if (attr(design[['manipulations']][[i]], 'has_order')) {
      n = attr(design[['orderings']][[i]], 'n')
      r = attr(design[['orderings']][[i]], 'r')
      ps = factorial(n) / factorial(n - r)
      stimulus <- names(design[['orderings']][i])
      new_printmsg <- paste0(new_printmsg,"  ", n, "-pick-", r,"=", ps, " permutations of ", stimulus,".\n")
    }
    else {
      n <-  length(design[['manipulations']][[i]])
      stimulus <- names(design[['manipulations']][i])
      new_printmsg <- paste0(new_printmsg,"  ", n, " levels of ", stimulus, "\n")
    }
  }
  attr(design[['counterbalance']], 'printmsg') <- new_printmsg
  design
  # 12 stimulus list total, counterbalanced by:
  # 2 levels of context
  # 3-pick-2=6 permutations of contour
}

# Creates latin square list assignments
.assign_latinsquare <- function(design){
  n_items <- attr(design[['items']], 'total')
  n_lists <- .calculate_lists(design)

  assignments <- purrr::list_along(1:n_items)
  ls_order <- 1:n_lists
  mod_n <- n_lists + 1

  # Procedure to create latinsquare groups
  for (i in 1:n_items) {
    assignments[[i]] <- ls_order
    ls_order <- (ls_order + 1) %% mod_n
    zero_index <- which(ls_order == 0)
    ls_order[zero_index] <- 1
  }
  unlist(assignments)
}
