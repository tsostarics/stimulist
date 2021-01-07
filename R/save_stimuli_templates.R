#' Save stimuli templates
#'
#' After adding stimuli with add_stimuli_by(), this will save .csv files for
#' you to fill in in excel. The completed files will be read in later to merge
#' into one large table that can be counterbalanced into different lists.
#'
#' @param .data A design
#'
#' @return
#' @export
#'
#' @examples
save_stimuli_templates <- function(.data){
  template <- .data[['stimuli']]
  filenames <- names(template)
  for (i in 1:length(template)) {
    this_file <- paste0(filenames[i], '.csv')
    write.csv(template[[i]], file = this_file, row.names = FALSE, na = '')
  }

  message(".csv files have been written for: ",
          paste(filenames, collapse = ", "))
}
