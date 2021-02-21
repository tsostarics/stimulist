#' Save stimuli templates
#'
#' After adding stimuli with add_stimuli_by(), this will save .csv files for
#' you to fill in in excel. The completed files will be read in later to merge
#' into one large table that can be counterbalanced into different lists.
#'
#' @param design Experiment design
#' @param as_workbook Whether to save as an excel workbook or not
#'
#' @return
#' @export
#' @importFrom utils write.csv
#' @examples
save_stimuli_templates <- function(design, as_workbook = TRUE) {

  stimuli <- design[["stimuli"]]
  filenames <- names(stimuli)

  if (as_workbook) {
    requireNamespace("xlsx", quietly = TRUE)
    wb <- xlsx::createWorkbook()
    for (i in 1:length(stimuli)) {
      sheet <- xlsx::createSheet(wb, filenames[i])
      to_write <- merge(get_stim_table(design), stimuli[[i]], all = F, allow.cartesian = T) ## change to merge later
      to_write$tojoin <- NULL
      xlsx::addDataFrame(to_write, sheet = sheet, row.names = FALSE)
    }
    xlsx::saveWorkbook(wb, "experiment_stimuli.xlsx")
    message("An excel workbook has been saved with sheets for each stimulus component.")
  } else {
    for (i in 1:length(stimuli)) {
      this_file <- paste0(filenames[i], ".csv")
      write.csv(to_write, file = this_file, row.names = FALSE, na = "")
    }
    message(
      ".csv files have been written for: ",
      paste(filenames, collapse = ", ")
    )
  }
}
