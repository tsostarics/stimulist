#' Create Design conditions
#'
#' @param design
#'
#' @return A data frame
#' @export
create_template <- function(design){
  # Cross all of our conditions
  conditions <- expand.grid(design[['manipulations']])
  conditions$id <- 1 # for joining

  # Create trial information for each condition
  trials <- get_stim_table(design)

  template <- left_join(conditions, trials, by = 'id')
  template$id <- NULL
  template <- set_filepaths(design, template)
  template
}
