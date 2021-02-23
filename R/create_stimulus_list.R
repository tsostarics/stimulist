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
          merge(x, y, all = F)
        },
        design[["presentations"]]
      ),
      all = F,
      allow.cartesian = T
    )
  expanded[['tojoin']] <- NULL
  expanded
}
