#' Counterbalance experiment
#'
#' This will counterbalance the entire experiment using a latin square approach
#' by default. Right now it's actually the only method, but in the future this
#' can be expanded further. Future functionality will also allow for different
#' types of stimuli to be counterbalanced separately (eg, critical trials vs
#' filler trials).
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
  design
}

# Get the number of lists we need to make while preventing double counting
# when incorporating crossed manipulations
.calculate_lists <- function(design){
  presentations <- names(design[['presentations']])
  crossed_presentations <- presentations[grepl(' x ', presentations)]
  manips_to_check <- c(str_split(crossed_presentations, " x ")[[1]],
                       'constant_for_all')
  reduced_presentations <- presentations[!presentations %in% manips_to_check]

  prod(vapply(design[['presentations']][reduced_presentations], nrow, 1L))
}

# Creates latin square list assignments
.assign_latinsquare <- function(design){
  n_trials <- attr(design[['trials']], 'total')
  n_lists <- .calculate_lists(design)

  assignments <- purrr::list_along(1:n_trials)
  ls_order <- 1:n_lists
  mod_n <- n_lists + 1

  # Procedure to create latinsquare groups
  for (i in 1:n_trials) {
    assignments[[i]] <- ls_order
    ls_order <- (ls_order + 1) %% mod_n
    zero_index <- which(ls_order == 0)
    ls_order[zero_index] <- 1
  }
  unlist(assignments)
}
