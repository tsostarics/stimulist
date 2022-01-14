#' Save JSON files
#'
#' This function does the same as save_lists, but in json format instead of
#' csv format. It will write .js files that you can import directly into your
#' experiment. Stimuli that present multiple levels on a single trial will be
#' saved as an object with the stimulus name. For example, audio_1  = x
#' and audio_2 = y will be converted into audio = {audio_1 = x, audio_2 = y}.
#' If x and y are greater than length 1 for some reason, they will be converted
#' to an an array within the audio object.
#'
#' If you use a .js extension, it will save a javascript file with the list
#' saved to the object name given with objectname. Using .json will prevent this.
#'
#' @param design Experimental design
#' @param filename Filename you want to use for each list (number will be appended)
#' @param objectname Name you want to use for the js object in the file
#' @param separate_items Should different item types be saved in separate files
#' @param as_one_file Should all stimuli be saved as one file (NOT recommended)
#' @param extension File extension, defaults to .js
#' @param remove_type Should the type column be removed from the output file? If
#' TRUE and there's only 1 type, the item column is renamed to the 1 value of the
#' type column. If FALSE, type column is retained. If a string, will remove the
#' type column and rename the item column to the given string.
#'
#' @export
save_json <- function(design,
                      filename = gsub(" ", "_", design[["name"]]),
                      objectname = "stimulist",
                      extension = ".json",
                      separate_items = FALSE,
                      as_one_file = FALSE,
                      remove_type = FALSE) {
  requireNamespace("jsonlite", quietly = TRUE)
  counterbalanced <- "counterbalance" %in% names(design[["complete_experiment"]])

  nested_cols <- .get_nested_cols(design)
  nested_df <- .nest_columns(design[["complete_experiment"]], nested_cols)

  if (!counterbalanced | as_one_file) {
    .save_json_list(nested_df, filename, objectname, extension, remove_type)
    message(paste0("Successfully wrote 1 ", extension, " file."))
    return(invisible(NULL))
  }

  # Split the individual trial lists up
  design <- split_lists_by(design, 'counterbalance') # when did I do this..?
  splits <- attr(design[['counterbalance']], "splits")
  lists <- .split_stimulist(nested_df, splits, separate_items)
  n_lists <- length(lists)
  file_labels <- .get_file_labels(design, n_lists, separate_items)

  for (i in seq_len(n_lists)) {
    current_type <- ifelse(separate_items, lists[[i]][["type"]][[1L]], "")
    current_type <- ifelse(current_type == "", "", paste0(current_type, "_"))
    file_label <- file_labels[[i]]
    out_file <- paste0(filename, "_", current_type, file_label)

    .save_json_list(lists[[i]],
                    filename = out_file,
                    objectname = paste(objectname, current_type, file_label, sep = "_"),
                    extension,
                    remove_type = FALSE
    )
  }

  message(paste0("Successfully wrote ", n_lists, " ", extension, " files."))
  return(invisible(NULL))
}

.save_json_list <- function(list_df, filename, objectname, extension, remove_type) {
  list_df <- .remove_type_column(list_df, remove_type)
  json_data <- jsonlite::toJSON(list_df, auto_unbox = TRUE, pretty = TRUE)
  filename <- paste0(filename, extension)
  fileConn <- file(filename, "w") # Must denote w or append won't work
  to_append = TRUE
  if (extension == ".js") {
    write(x = paste0(objectname, " = "), file = fileConn)
    to_append = FALSE
  }
  write(x = json_data, file = fileConn, append = to_append)
  close(fileConn)
}

.get_nested_cols <- function(design) {
  # Returns which variables have _n to denote that they're part of a permutation
  stimuli <- unlist(
    lapply(
      design[["stimuli"]],
      names
    ),
    use.names = FALSE
  )

  presentations <- unlist(
    lapply(
      design[["presentations"]],
      names
    ),
    use.names = FALSE
  )

  stimuli[!stimuli %in% presentations]
}


.nest_columns <- function(full_table, group_cols) {
  # Takes columns like audio_1 and audio_2 and combines them into a
  # list column called audio. Allows us to export an embedded js object.
  if (identical(character(0), group_cols))
    return(full_table) # handles case where there's no fancy nesting
  nested_dfs <-
    lapply(
      group_cols,
      function(x) {
        dplyr::transmute(full_table,
                         !!x := purrr::transpose(
                           full_table[grepl(paste0("^", x), names(full_table))] # Transmute won't let us use .keep here
                         )
        )
      }
    )

  cbind(
    dplyr::select(
      full_table,
      !tidyselect::starts_with(group_cols)
    ),
    nested_dfs
  )
}
