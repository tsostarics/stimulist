#' Save stimulus lists
#'
#' Given a counterbalanced experiment from fill_experiment(), saves the individual
#' counterbalanced lists. In the future there will be an option for JSON files
#' or csv files, but for now it does csv files.
#'
#' @param design Experiment design
#' @param filename Name to use for output files
#' @param separate_items Whether to save separate files for different types of
#' items (eg critical and filler lists are saved separately)
#' @param path Directory to save files into (default is getwd())
#' @param as_one_file Whether you want to save all trials as 1 file
#'
#' @export
save_lists <- function(design,
                       filename = "experiment",
                       path = getwd(),
                       separate_items = FALSE,
                       as_one_file = FALSE) {
  # Error handling
  if (!"complete_experiment" %in% names(design)) {
    stop("Please use fill_experiment() before trying to save lists")
  }
  splits <- attr(design[['counterbalance']], "splits")
  if (!"counterbalance" %in% names(design[["complete_experiment"]]) & is.null(splits)) {
    warning("No counterbalancing has been set, this will save only one list.")
    write.csv(design[["complete_experiment"]],
      paste0(path, "/", filename, ".csv"),
      row.names = F
    )
    return(NULL)
  }

  if (as_one_file) {
    write.csv(design[["complete_experiment"]],
      "stimulus_list.csv",
      row.names = F
    )
    message("Wrote 1 .csv file with all stimuli successfully.")
    return(NULL)
  }

  groups <- "counterbalance"
  if (!is.null(splits))
    groups <- splits
  if (separate_items)
    groups <- c(groups, "type")

  lists <-
    dplyr::group_split(
      dplyr::group_by(
        design[["complete_experiment"]],
        !!!syms(groups)
      )
    )

  n_types <- length(unique(design[["complete_experiment"]][["type"]]))
  n_lists <- length(lists)
  file_labels <- seq_len(n_lists)
  if (separate_items) {
    file_labels <- ceiling(file_labels / n_types)
  } # Allows for better numbering

  for (i in seq_len(n_lists)) {
    current_list <- lists[[i]]
    trial_type <- ifelse(separate_items, paste0(current_list[["type"]][[1L]], "_"), "")

    # note: maybe there should be a check to see if the / is included. getwd
    # doesn't include it but a user might provide it manually.
    write.csv(current_list,
      paste0(path, "/", trial_type, filename, "_", file_labels[i], ".csv"),
      na = "",
      row.names = F
    )
  }
  message(paste0("Successfully wrote ", length(lists), " lists."))
  NULL
}
