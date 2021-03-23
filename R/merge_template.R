#' Merge settings from template
#'
#' After using save_stimulus_template() to create a blank excel workbook, you
#' can then fill it out with whatever file names or text you need. But,
#' you'll probably want to load that back into the design you made so you can
#' save counterbalanced lists of what you created. This function will fill in
#' any empty columns in the complete_experiment table with what you put into
#' the blank template file. The function will automatically handle cases where
#' you have permutations of orders.
#'
#' @param design Experiment design
#' @param template_path Path to template file
#'
#' @export
merge_template <- function(design, template_path) {
  requireNamespace("xlsx", quietly = TRUE)
  all_stimuli <- design[["complete_experiment"]]
  template_sheets <- .read_template(template_path)

  # Get the columns we need to fill, and what we're going to merge by
  to_fill <- .get_fillable(all_stimuli)
  to_fill <- to_fill[unlist(to_fill)]
  merge_keys <- names(dplyr::select(all_stimuli, !tidyselect::contains(names(to_fill))))

  for (i in seq_len(length(template_sheets))) {
    sheet <- template_sheets[[i]]
    sheet_cols <- names(sheet)
    # This value(s) will need to be excluded from the stimuli table for joining
    exclude_var <- names(dplyr::select(to_fill, tidyselect::contains(sheet_cols)))
    template_value <- stringr::str_match(exclude_var[1L], "^(.+)_\\d$")[[2]]

    if (!any(sheet_cols %in% names(to_fill)))
      next
    if (length(exclude_var) == 1 | is.na(template_value)) {
      all_stimuli <-
        merge(
          dplyr::select(
            all_stimuli,
            !tidyselect::contains(exclude_var)
          ),
          dplyr::select(sheet, -"trial")
        )
    } else {
      # We need to join the same column on different key columns to handle
      # instances where present_n_of() has been used.
      template_key <- sheet_cols[(!sheet_cols %in% merge_keys) & (!sheet_cols == template_value)]
      table_keys <- names(dplyr::select(all_stimuli, tidyselect::contains(template_key)))
      for (j in seq_len(length(exclude_var))) {
        sheet[exclude_var[j]] <- sheet[template_value]
        sheet[table_keys[j]] <- sheet[template_key]
        all_stimuli <- merge(
          dplyr::select(sheet, -tidyselect::all_of(c(template_key, template_value))),
          dplyr::select(all_stimuli, !tidyselect::contains(exclude_var[j]))
        )
        sheet[exclude_var[j]] <- NULL
        sheet[table_keys[j]] <- NULL
      }
    }
  }
  printmsg <-
    paste0(attr(design[["complete_experiment"]], 'printmsg'),
           "Merged values from ", template_path, "\n")
  design[["complete_experiment"]] <- all_stimuli
  attr(design[["complete_experiment"]], 'printmsg') <- printmsg
  design
}

.get_fillable <- function(filled_table) {
    dplyr::summarize(filled_table,
                     dplyr::across(tidyselect::everything(),
                                   function(x)
                                     data.table::first(is.na(x))
                                   )
                     )
}

.read_template <- function(template_path) {
  sheets <- xlsx::getSheets(xlsx::loadWorkbook(template_path))
  sheet_names <- names(sheets)
  rm(sheets)

  sheet_dfs <- lapply(sheet_names, function(x) xlsx::read.xlsx(template_path, sheetName = x))
  names(sheet_dfs) <- sheet_names
  sheet_dfs
}
