create_template <- function(design){
  template <- expand.grid(design[['manipulations']])
  template$id <- 1

  trials <- stimulus_table(design)

  template <- left_join(template, trials, by = 'id')
  template$id <- NULL
  template <- set_filepaths(design, template)
  template
}
