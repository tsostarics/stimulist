#' Add Experimental Manipulations
#'
#' @param design Experiment design
#' @param ... A series of named arguments with manipulation levels
#'
#' @return
#' @export
#'
#' @examples
add_manipulations <- function(design, ...) {
  if ("" %in% names(list(...)))
    stop("Must provide named arguments.")
  design[["manipulations"]] <- list(...)
  for (i in 1:length(design[["manipulations"]])) {
    attr(design[["manipulations"]][[i]], "has_order") <- FALSE
  }

  .set_manipulation_printmsg(design)
}

.set_manipulation_printmsg <- function(design) {
  new_printmsg <- "Manipulations:\n"

  # this could be rewritten with vapply, to do later
  for (i in 1:length(design[["manipulations"]])) {
    new_printmsg <-
      paste0(
        new_printmsg,
        paste0(
          "  ",
          names(design[["manipulations"]][i]),
          " (",
          length(design[["manipulations"]][[i]]),
          "): ",
          paste0(design[["manipulations"]][[i]], collapse = " "),
          "\n"
        )
      )
  }
  attr(design$manipulations, "printmsg") <- new_printmsg
  design
}
