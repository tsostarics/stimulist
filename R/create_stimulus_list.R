#' Create stimulus list
#'
#' @param design Experiment design
#'
#' @export
create_stimulus_list <- function(design) {
  stim_table <- get_stim_table(design)
  presentations <- design$presentations

  merged_presentations <-
    Reduce(
      function(x, y) {
        reduced <- dplyr::left_join(x, y)
        reduced$tojoin <- 1
        reduced
      },
      presentations
    )

  output <- dplyr::left_join(stim_table, merged_presentations, by = "tojoin", keep = FALSE)
  output$tojoin <- NULL
  output
}
