#' Save stimuli templates
#'
#' After adding stimuli with add_stimuli_by(), this will save .csv files for
#' you to fill in in excel. The completed files will be read in later to merge
#' into one large table that can be counterbalanced into different lists.
#'
#' @param design Experiment design
#' @param as_workbook Whether to save as an excel workbook or not
#'
#' @export
#' @importFrom utils write.csv
save_stimuli_templates <- function(design, as_workbook = TRUE) {
  exp_name <- paste0(design[["name"]],"_stimuli")
  stimuli <- design[["stimuli"]]
  filenames <- names(stimuli)

  if (as_workbook) {
    requireNamespace("xlsx", quietly = TRUE)
    wb <- xlsx::createWorkbook()
    for (i in seq_len(length(stimuli))) {
      # Load a worksheet for this stimulus
      sheet <- xlsx::createSheet(wb, filenames[i])
      # Read the glueformula attribute from each stimulus
      glueformula <- attr(stimuli[[i]], "glueformula")
      # Expand the full set of stimuli we need
      to_write <- merge(get_stim_table(design),
                        stimuli[[i]], all = F,
                        allow.cartesian = T)
      to_write <- merge(.get_label_table(design),
                        to_write,
                        by = c('item','type'),
                        all = F,
                        allow.cartesian = T)
      to_write$tojoin <- NULL
      # If there's a glue formula already specified, fill the column with it
      if (!is.null(glueformula))
        to_write[[glueformula[[2]]]] <-  glue::glue(glueformula[[3]], .envir = to_write)
      xlsx::addDataFrame(to_write, sheet = sheet, row.names = FALSE)
    }
    xlsx::saveWorkbook(wb, paste0(exp_name, ".xlsx"))
    message("An excel workbook has been saved with sheets for each stimulus component.")
  } else {
    for (i in seq_len(length(stimuli))) {
      this_file <- paste0(filenames[i], ".csv")
      write.csv(to_write, file = this_file, row.names = FALSE, na = "")
    }
    message(
      ".csv files have been written for: ",
      paste(filenames, collapse = ", ")
    )
  }
}
