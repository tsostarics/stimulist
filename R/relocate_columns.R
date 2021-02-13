#' Relocate columns in completed experiment
#'
#' This is just a wrapper around dplyr::relocate to manipulate the ordering
#' of columns in the complete_experiment entry. Right now it doesn't allow for
#' the use of .before and .after, but I'll fix that later.
#'
#' @param design
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
relocate_columns <- function(design, ...){
  if (!'complete_experiment' %in% names(design))
    stop("Complete experiment not found, please run fill_experiment() first")
  dots <- vapply(enexprs(...), as.character, 'char')
  design[['complete_experiment']] <-
    dplyr::relocate(design[['complete_experiment']],
                    all_of(dots))
  design
}
