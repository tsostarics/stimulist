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
glue_filenames_by <- function(design, ..., as_levels = FALSE){
  formulas <- lapply(enexprs(...), as.character)
  if (!all(vapply(formulas, length, 1L) == 3))
    stop("You must provide formulas of the style `varname ~ string`")
  else if (any(vapply(formulas, function(x) any(grepl(" [*+] ", x)), T)))
    stop("RHS of formula should only have a single string, not multiple with + or *.")
  else if (rlang::is_na(design[['complete_experiment']])) # i might change this to be is_na where complete_experiment is specified in the constructor
    stop("You need to run fill_experiment() before glue_filenames_by()")

  # basically i think you need more logic to do the following:
  # - parse the glue string for variables in brackets

  # - check if any of those variables have orderings
  # - if they have orderings, then create new versions of the glue string that
  #   tacks on the _1 and _2 etc as they correspond to those from fill_vars
  # - caveat: only do this for the ones that matter! eg contour_1 and contour_2
  #   but not context.

  # note: you can implement levels by changing the glue string to look like
  # "sound/{as.integer(factor(context))} instead of just "sound/{context}.
  # but consider adding a print msg that this is occuring.

  for (f in formulas) {
    glue_vars <- .extract_gluevars(f[[3]])
    glue_dependencies <- .check_orderings(design, glue_vars)
    fill_vars <- .get_containing_vars(design[['complete_experiment']], f[[2]])
    glue_strings <- .expand_glue(f[[3]], glue_dependencies, fill_vars, as_levels)

    for (i in 1:length(fill_vars)) {
      design[['complete_experiment']][[fill_vars[i]]] <- glue::glue(glue_strings[i], .envir = design[['complete_experiment']])
    }
  }
  .set_glue_printmsg(design, formulas, as_levels)
}

.expand_glue <- function(glue_string, varname, to_fill, as_levels){
  bracket <- ifelse(as_levels, r"())})", "}")
  appendages <- paste0(str_extract(to_fill, "_\\d+$"), bracket)

  regex_pattern <- paste0("(?<=", paste0(varname, collapse = "|"),r"()})")
  ordered_gluestrings <-
    vapply(appendages,
           function(x)
             gsub(regex_pattern, x, glue_string, perl = T),
           FUN.VALUE = "char",
           USE.NAMES = F)

  if (as_levels) {
    glue_vars <- paste0(.extract_gluevars(glue_string), collapse = "|")
    factor_regex <- paste0(r"({(?=)", glue_vars,")")
    closing_regex <- paste0(r"((?<=)", glue_vars,")}")
    ordered_gluestrings <-
      vapply(ordered_gluestrings,
             function(x)
               gsub(closing_regex,
                    r"())})",
                    gsub(factor_regex,
                         r"({as.integer(factor()",
                         x,
                         perl = T
                    ),
                    perl = T
               ),
             FUN.VALUE = "char",
             USE.NAMES = F)
  }
  ordered_gluestrings
}

.extract_gluevars <- function(gluestring){
  str_match_all(gluestring, "\\{([^\\}]+)\\}")[[1L]][,2]
}

.check_orderings <- function(design, gluevars){
  have_orderings <- vapply(gluevars, function(x) x %in% names(design[['orderings']]), TRUE)
  gluevars[have_orderings]
}

.set_glue_printmsg <- function(design, formulas, as_levels){
  new_printmsg <- paste0(attr(design[['complete_experiment']], 'printmsg'),
                         "Filenames specified")
  new_printmsg <- ifelse(as_levels,
                         paste0(new_printmsg,
                                " (using numbering by alphabetical order of levels):\n"),
                         paste0(new_printmsg,
                                ":\n"))
  formula_specifications <-
    vapply(formulas,
           function(x) paste0("  ",x[[2]], " values glued with: ", x[[3]], "\n"), "char")
  new_printmsg <-
    paste0(new_printmsg, paste0(formula_specifications, collapse = ""))
  attr(design[['complete_experiment']], 'printmsg') <- new_printmsg
  design
}

.get_containing_vars <- function(completed_df, var){
  output <- names(completed_df)[grepl(var, names(completed_df))]
  if (identical(output, character(0)))
    stop(paste0("`", var, "` not found in the experimental design."))
  output
}
