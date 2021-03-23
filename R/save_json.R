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
#' @param design Experimental design
#' @param filename Filename you want to use for each list (number will be appended)
#' @param objectname Name you want to use for the js object in the file
#' @param separate_items Should different item types be saved in separate files
#' @param as_one_file Should all stimuli be saved as one file (NOT recommended)
#'
#' @export
save_json <- function(design,
                      filename = gsub(" ", "_", design[["name"]]),
                      objectname = "stimulist",
                      separate_items = FALSE,
                      as_one_file = FALSE) {
  requireNamespace("jsonlite", quietly = TRUE)
  counterbalanced <- "counterbalance" %in% names(design[["complete_experiment"]])

  nested_cols <- .get_nested_cols(design)
  nested_df <- .nest_columns(design, nested_cols)

  if (!counterbalanced | as_one_file) {
    .save_json_list(nested_df, filename, objectname)
    message("Successfully wrote 1 .js file.")
    invisible(NULL)
  }
  splits <- attr(design[['counterbalance']], "splits")
  lists <- .split_stimulist(nested_df, splits, separate_items)
  n_lists <- length(lists)
  file_labels <- .get_file_labels(design, n_lists, separate_items)

  for (i in seq_len(n_lists)) {
    current_type <- ifelse(separate_items, lists[[i]][["type"]][[1L]], "")
    file_label <- file_labels[[i]]
    out_file <- paste0(filename, "_", current_type, "_", file_label, ".js")

    .save_json_list(lists[[i]],
                    filename = out_file,
                    objectname = paste(objectname, current_type, file_label, sep = "_")
    )
  }

  message(paste0("Successfully wrote ", n_lists, " .js files."))
  invisible(NULL)
}

.save_json_list <- function(list_df, filename, objectname) {
  json_data <- jsonlite::toJSON(list_df, auto_unbox = T, pretty = T)
  fileConn <- file(filename, "w") # Must denote w or append won't work
  write(x = paste0(objectname, " = "), file = fileConn)
  write(x = json_data, file = fileConn, append = TRUE)
  close(fileConn)
}

.get_nested_cols <- function(design) {
  # Returns which variables have _n to denote that they're part of a permutation
  stimuli <- unlist(
    lapply(
      design[["stimuli"]],
      names
    ),
    use.names = F
  )

  presentations <- unlist(
    lapply(
      design[["presentations"]],
      names
    ),
    use.names = F
  )

  stimuli[!stimuli %in% presentations]
}


.nest_columns <- function(design, group_cols) {
  # Takes columns like audio_1 and audio_2 and combines them into a
  # list column called audio. Allows us to export an embedded js object.
  nested_dfs <-
    lapply(
      group_cols,
      function(x) {
        design[["complete_experiment"]] %>%
          dplyr::transmute(!!x := purrr::transpose(
            .[grepl(paste0("^", x), names(.))] # Mutate won't let use use .keep here
          )
          )
      }
    )

  cbind(
    dplyr::select(
      design[["complete_experiment"]],
      !tidyselect::starts_with(group_cols)
    ),
    nested_dfs
  )
}
