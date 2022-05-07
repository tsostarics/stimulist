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
#' @param remove_type Should the type column be removed from the output file? If
#' TRUE and there's only 1 type, the item column is renamed to the 1 value of the
#' type column. If FALSE, type column is retained. If a string, will remove the
#' type column and rename the item column to the given string.
#'
#' @export
save_lists <- function(design,
                       filename = design[["name"]],
                       path = getwd(),
                       separate_items = FALSE,
                       as_one_file = FALSE,
                       remove_type = FALSE) {
  # Error handling
  if (!"complete_experiment" %in% names(design)) {
    stop("Please use fill_experiment() before trying to save lists")
  }

  counterbalanced <- "counterbalance" %in% names(design[["complete_experiment"]])

  nested_cols <- .get_nested_cols(design)
  nested_df <- .nest_columns(design[["complete_experiment"]], nested_cols)

  if (!counterbalanced | as_one_file) {
    output_table <- .remove_type_column(design[["complete_experiment"]], remove_type)
    write.csv(output_table,
              "stimulus_list.csv",
              row.names = FALSE
    )
    message("Wrote 1 .csv file with all stimuli successfully.")
    return(invisible(NULL))
  }

  design <- split_lists_by(design, 'counterbalance') # when did I do this..?
  splits <- attr(design[['counterbalance']], "splits")
  lists <- .split_stimulist(design[["complete_experiment"]], splits, separate_items)
  n_lists <- length(lists)
  file_labels <- .get_file_labels(design, n_lists, separate_items)


  for (i in seq_len(n_lists)) {
    current_list <- lists[[i]][order(lists[[i]][['trial']]),]
    trial_type <- ifelse(separate_items, paste0(current_list[["type"]][[1L]], "_"), "")

    write.csv(current_list,
              .make_path(path, trial_type, filename, file_labels, i),
              na = "",
              row.names = FALSE
    )
  }
  message(paste0("Successfully wrote ", length(lists), " lists."))
  return(invisible(NULL))
}

.make_path <- function(path, trial_type, filename, file_labels, i) {
  path <-  gsub("/?$","/",path)
  paste0(path, trial_type, filename, "_", file_labels[i], ".csv")
}

.split_stimulist <- function(design_exp, splits, separate_items, remove_type = FALSE) {
  if (is.null(splits))
    return(list(design_exp))

  groups <- "counterbalance"
  if (!is.null(splits))
    groups <- splits
  if (separate_items)
    groups <- c(groups, "type")
  design_exp <- .remove_type_column(design_exp, remove_type)

  lists <-
    dplyr::group_split(
      dplyr::group_by(
        design_exp,
        !!!syms(groups)
      )
    )

  lists
}

.get_file_labels <- function(design, num_lists, separate_items) {
  n_types <- length(unique(design[["complete_experiment"]][["type"]]))
  file_labels <- seq_len(num_lists)
  if (separate_items) {
    file_labels <- ceiling(file_labels / n_types)
  } # Allows for better numbering

  file_labels
}


.remove_type_column <- function(design_tbl, remove_type = FALSE){
  if (remove_type == TRUE) {
    if (length(unique(design_tbl[['type']])) > 1)
      stop("Number of types is greater than one, must specify column name as string")
    replace_string <- design_tbl[['type']][[1L]]
  } else if (is.character(remove_type)) {
    if (length(remove_type) > 1)
      stop("Must provide a character vector of length 1 for remove_type")
    replace_string <- remove_type
  } else{
    return(design_tbl)
  }

  which_items <- names(design_tbl) == "item"
  names(design_tbl)[which_items] <- replace_string
  design_tbl['type'] <- NULL
  design_tbl
}





