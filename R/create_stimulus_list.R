#' Create stimulus list
#'
#' @param design Experiment design
#'
#' @export
create_stimulus_list <- function(design) {
  stim_table <- get_stim_table(design)
  expanded <-
    merge(
      stim_table,
      Reduce(
        function(x, y) {
          merge(x, y, all = FALSE)
        },
        design[["presentations"]]
      ),
      all = FALSE,
      allow.cartesian = TRUE
    )
  expanded[['tojoin']] <- NULL
  expanded
}
