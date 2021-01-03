add_manip <- function(.data, ...){
  .data[['manipulations']] <-  list(...)
  .data
}
