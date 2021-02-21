#' Create table for stimuli
#'
#' @param design Experiment design
#' @param add_id Whether to add an id to join by
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom data.table rbindlist
get_stim_table <- function(design, add_id = T) {
  out <-
    rbindlist(
      purrr::lmap(
        design[["items"]],
        function(x) {
          list(data.frame(stimulus = 1:x[[1L]], type = names(x)))
        }
      )
    )
  out[["trial"]] <- 1:nrow(out)
  if (add_id) out[["tojoin"]] <- 1
  out
}
