
#' Create presentation of crossed manipulations
#'
#' Helper to make a crossed presentation of stimuli
#'
#' @param design Stimulist object
#' @param manipulations Vector of strings of manipulation names
#' @param columns Columns to add
#'
.make_crossed_presentation <- function(design, manipulations, columns) {
  crossed_manipulations <- .cross_manipulations(design, manipulations)
  order_nums <- as.integer(str_extract(names(crossed_manipulations), "\\d$"))
  none_specified <- all(is.na(order_nums))

  if (!none_specified) {
    order_nums <- seq_len(max(order_nums, na.rm = TRUE))
    numbered_cols <- unlist(lapply(columns, FUN = function(x) paste(x, order_nums, sep = "_")))
    add_cols <- .make_singleton_columns(numbered_cols)
  }
  else {
    add_cols <- .make_singleton_columns(columns)
  }
  cbind(crossed_manipulations, add_cols)
}

#' Create presentation of uncrossed manipulations
#'
#' Helper to make a presentation of stimuli
#'
#' @param design Stimulist object
#' @param manipulation Manipulation names
#' @param columns Columns to add
#'
.make_presentation <- function(design, manipulation, columns) {
  # Extract ordering for this manipulation, and get the number to present
  ordering <- design[["orderings"]][[manipulation]]
  order_nums <- seq_len(length(ordering))
  # For each of the columns to add, append 1:order_nums
  add_cols <- unlist(lapply(columns,FUN = function(x) paste(x, order_nums, sep = "_")))

  # Turn the vector of columns to add into a named list for cbind
  add_cols <- .make_singleton_columns(add_cols)

  output <- cbind(ordering, add_cols)
  output[["tojoin"]] <- 1
  output
}


#' Create stimulus presentation
#'
#' @param design Stimulist object
#' @param formula_manipulation Vector of strings of manipulation names
#' @param formula_columns Columns to add
#' @param manipulation_stimuli Expanded grid of stimuli
#' @param is_crossed Boolean if should be crossed presentation
#' @param is_ordered Boolean if uncrossed presentation has ordering
#'
.add_stimulus_presentation <- function(design,
                                       formula_manipulation,
                                       formula_columns,
                                       manipulation_stimuli,
                                       is_crossed,
                                       is_ordered) {
  if (is_crossed)
    return(.make_crossed_presentation(design, formula_manipulation, formula_columns))
  if (is_ordered)
    return(.make_presentation(design, formula_manipulation, formula_columns))
  return(manipulation_stimuli)
}
