#' Cross manipulations
#'
#' Helper to cross the levels of two or more manipulations that affect the value
#' on a trial
#'
#' @param design Stimulist object
#' @param manipulations Vector of strings of manipulation names
#'
.cross_manipulations <- function(design, manipulations) {
  manipulations <- strsplit(manipulations, " x ")[[1]]
  is_ordered <- manipulations %in% names(design[["orderings"]])
  ordered_manipulations <- design[["orderings"]][manipulations[is_ordered]]
  unordered_manipulations <- design[["manipulations"]][manipulations[!is_ordered]]

  unordered_dfs <-
    purrr::lmap(
      unordered_manipulations,
      function(x) {
        outdf <- as.data.frame(x)
        outdf[["tojoin"]] <- 1
        list(outdf)
      }
    )

  ordered_dfs <-
    purrr::lmap(
      ordered_manipulations,
      function(x) {
        x[[1]][["tojoin"]] <- 1
        x
      }
    )


  merged_ordered <- Reduce(function(x, y) merge(x, y, by = "tojoin", all = T), ordered_dfs)
  merged_unordered <- Reduce(function(x, y) merge(x, y, by = "tojoin", all = T), unordered_dfs)

  NO_ORDERED <- is.null(merged_ordered)
  NO_UNORDERED <- is.null(merged_unordered)

  if (NO_ORDERED & NO_UNORDERED)
    stop("No variables provided or invalid variables")

  if (NO_ORDERED) return(merged_unordered)
  if (NO_UNORDERED) return(merged_ordered)

  merge(merged_unordered, merged_ordered, by = "tojoin", all = T)
}
