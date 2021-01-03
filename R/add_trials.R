
add_trials <- function(.data, ...){
  .data[['trials']] <- list(...)
  attributes(.data[['trials']])$total <- sum(unlist(.data[['trials']]))
  .data
}
