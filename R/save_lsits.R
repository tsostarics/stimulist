#' Save stimulus lists
#'
#' Given a counterbalanced experiment from fill_experiment(), saves the individual
#' counterbalanced lists. In the future there will be an option for JSON files
#' or csv files, but for now it does csv files.
#'
#' @param design
#' @param filename
#' @param path
#'
#' @return
#' @export
#'
#' @examples
save_lists <- function(design, filename="experiment", path=getwd()){
  if (!'complete_experiment' %in% names(design))
    stop("Please use fill_experiment() before trying to save lists")
  if (!'counterbalance' %in% names(design[['complete_experiment']])) {
    warning("No counterbalancing has been set, please use counterbalance()")
    write.csv(design[['complete_experiment']], paste0(path, "/", filename, ".csv"))
    return(NULL)
  }

  lists <- dplyr::group_split(
    dplyr::group_by(
      design[['complete_experiment']],
      counterbalance
    )
  )

  for (i in 1:length(lists)) {
    current_list <- lists[[i]]
    write.csv(current_list,
              paste0(path, "/", filename,"_",i, ".csv"),
              na = "",
              row.names = F)
  }
  message(paste0("Successfully wrote ", length(lists), " lists."))
  NULL
}
