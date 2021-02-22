#' Add items to experiment design
#'
#' @param design Experiment design
#' @param ... a series of named arguments with manipulation levels
#'
#' @export
add_items <- function(design, ...) {
  design[["items"]] <- list(...)
  attr(design[["items"]], "total") <- sum(unlist(design[["items"]]))
  for (i in 1:length(design[["items"]])) {
    attr(design[["items"]][[i]], "labels") <- ""
    attr(design[["items"]][[i]], "labelled") <- FALSE
  }
  .set_items_printmsg(design)
}

.set_items_printmsg <- function(design) {
  new_printmsg <- paste0(attr(design$items, "total"), " items total:\n")

  for (i in 1:length(design[["items"]])) {
    new_printmsg <-
      paste0(
        new_printmsg,
        paste0(
          "  ",
          names(design[["items"]][i]),
          ": ",
          design[["items"]][[i]],
          "\n"
        )
      )
  }

  attr(design$items, "printmsg") <- new_printmsg
  design
}
