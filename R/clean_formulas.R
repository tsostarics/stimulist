#' Clean formulas
#'
#' Helper to merge formulas that should have been written together as one
#'
#' @param formulas List of formulas
.clean_formulas <- function(formulas) {
  formula_lengths <- vapply(formulas, length, 1L)
  if (any(formula_lengths > 3 | formula_lengths < 2)) {
    stop("Malformed formula")
  }

  no_lhs <- vapply(formulas, function(x) length(x) == 2, T)
  combined_onesided <- .combine_onesided_formulas(formulas, no_lhs)
  combined_twosided <- .combine_twosided_formulas(formulas, !no_lhs)

  c(combined_onesided, combined_twosided)
}

.combine_onesided_formulas <- function(formulas, which_no_lhs) {
  if (!any(which_no_lhs)) return(list())
  formula_strings <- vapply(formulas[which_no_lhs], function(x) x[[2]], "char")
  list(c("~",paste0(formula_strings,collapse = " + ")))
}

.combine_twosided_formulas <- function(formulas, which_have_lhs){
  lhs <- unique(vapply(formulas[which_have_lhs], function(x) x[[2]], "char"))
  # For each lhs, combine into one formula
  purrr::lmap(
    lhs,
    function(x) {
      # LHS given in 2nd element, RHS in 3rd if twosided
      rhs <- vapply(formulas,
                    function(y) ifelse(y[[2]] == x, y[[3]], character(0)),
                    "char"
      )
      rhs <- rhs[!is.na(rhs)]
      list(c("~", x, paste0(rhs, collapse = " + ")))
    }
  )
}
