#' Create table for stimuli
#'
#' @param design
#' @param add_id
#'
#' @return
#' @export
#'
#' @examples
get_stim_table <- function(design, add_id=T){
  out <-
    rbindlist(
      lmap(
        design[['stimuli']],
        function(x)
          list(data.frame(stimulus = x[[1L]], type = names(x)))
      )
    )
  out$trial <- 1:nrow(out)
  if (add_id) out$id <- 1
  out
}