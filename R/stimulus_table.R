#' Create table for stimuli
#'
#' @param design Experiment design
#' @param add_id Whether to add an id to join by
#'
#' @export
#' @importFrom data.table rbindlist
get_stim_table <- function(design, add_id = T) {
  out <-
    rbindlist(
      purrr::lmap(
        design[["items"]],
        function(x) {
          list(data.frame(item = seq_len(x[[1L]]), type = names(x)))
        }
      )
    )
  out[["trial"]] <- seq_len(nrow(out))
  if (add_id) out[["tojoin"]] <- 1
  out
}
