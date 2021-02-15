#' Glue filenames by glue string
#'
#' Requires the {glue} package. Similar syntax to add_stimuli_by(), but here
#' you're specifying a glue string for a particular stimulus. Glue strings
#' utilize {brackets} to evaluate an expression within a string. In this case,
#' you'll be replacing a label in brackets with its value in the
#' completed_experiment data frame. Eg:
#'
#' ... %>%
#' add_stimuli_by(manip1*manip2 ~ audio_file) %>%
#' fill_experiment() %>%
#' glue_filenames_by(audio_file ~ "sounds/{manip1}_{manip2}.wav)
#'
#' This must be run after using fill_experiment.
#'
#' @param design
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
glue_filenames_by <- function(design, ...){
  formulas <- lapply(enexprs(...), as.character)
  if (!all(vapply(formulas, length, 1L) == 3))
    stop("You must provide formulas of the style `varname ~ string`")
  else if (any(vapply(formulas, function(x) any(grepl(" [*+] ", x)), T)))
    stop("RHS of formula should only have a single string, not multiple with + or *.")
  else if (is.null(design[['complete_experiment']])) # i might change this to be is_na where complete_experiment is specified in the constructor
    stop("You need to run fill_experiment() before glue_filenames_by()")


  for (f in formulas) {
    fill_vars <- .get_containing_vars(design[['complete_experiment']], f[[2]])
    for (var in fill_vars) {
      print(f[[3]])
      design[['complete_experiment']][[var]] <- glue::glue(f[[3]], .envir = design[['complete_experiment']])
    }
  }
  .set_glue_printmsg(design, formulas)
}

.set_glue_printmsg <- function(design, formulas){
  new_printmsg <- "Filenames specified:\n"
  formula_specifications <-
    vapply(formulas,
           function(x) paste0("  ",x[[2]], " values glued with: ", x[[3]], "\n"), "char")
  new_printmsg <-
    paste0(new_printmsg, paste0(formula_specifications, collapse = ""))
  attr(design, 'glue_printmsg') <- new_printmsg
  design
}

.get_containing_vars <- function(completed_df, var){
  output <- names(completed_df)[grepl(var, names(completed_df))]
  if (identical(output, character(0)))
    stop(paste0("`", var, "` not found in the experimental design."))
  output
}
