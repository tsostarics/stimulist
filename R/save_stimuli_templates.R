#' Save stimuli templates
#'
#' After adding stimuli with add_stimuli_by(), this will save .csv files for
#' you to fill in in excel. The completed files will be read in later to merge
#' into one large table that can be counterbalanced into different lists.
#'
#' @param design A design
#'
#' @return
#' @export
#'
#' @examples
save_stimuli_templates <- function(design){
  stimuli <- design[['stimuli']]
  filenames <- names(stimuli)
  for (i in 1:length(stimuli)) {
    this_file <- paste0(filenames[i], '.csv')
    to_write <-
      dplyr::select(
        dplyr::left_join(get_stim_table(design), stimuli[[i]]),
        -tojoin
        )
    write.csv(to_write, file = this_file, row.names = FALSE, na = '')
  }

  message(".csv files have been written for: ",
          paste(filenames, collapse = ", "))
}
