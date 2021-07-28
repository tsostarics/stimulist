#' Add Stimuli by Manipulations
#'
#' @param design Design
#' @param ... A series of formulas where the left hand side is a manipulation
#' and the right hand side is a series of variable names
#'
#' @export
#' @importFrom stats setNames
add_stimuli_by <- function(design, ...) {
  formulas <- .clean_formulas(lapply(enexprs(...), as.character))
  .check_if_contains(design, formulas)
  for (stimset in formulas) {
    if (length(stimset) == 2L) {
      design <- .add_constant_manipulation(design, stimset)
    } else {
      design <- .add_varying_manipulation(design, stimset)
    }
  }
  .set_stimuli_printmsg(design)
}

.add_constant_manipulation <- function(design, stimset){
  formula_columns <- strsplit(stimset[[2L]], " \\+ ")[[1L]]
  add_cols <- .make_singleton_columns(formula_columns)
  manipulation_stimuli <- cbind(data.frame(tojoin = 1), add_cols)
  design[["stimuli"]][["item_constants"]] <- manipulation_stimuli
  design[["presentations"]][["item_constants"]] <- manipulation_stimuli
  .set_stimulus_printmsg(design, "item_constants", formula_columns)
}

.add_varying_manipulation <- function(design, stimset) {
  # For stimuli that vary by a manipulation (manip ~ x + y + z...)
  # Get the manipulation name, cross by number of items, set name
  formula_manipulation <- stimset[[2L]] # Manipulation from lhs of formula
  is_crossed <- grepl(" \\* ", formula_manipulation)
  formula_cols <- strsplit(as.character(stimset[[3L]]), " \\+ ")[[1L]]
  # Create grid for the manipulation
  grid <- .create_grid(design, formula_manipulation)

  manipulation_name <- gsub(" \\* ", " x ", formula_manipulation) # change * to x for filenames
  is_ordered <- attr(design[["manipulations"]][[manipulation_name]], "has_order")
  # Make empty singleton columns to bind to output
  add_cols <- .make_singleton_columns(formula_cols)
  manipulation_stimuli <- cbind(grid, add_cols)
  design[["stimuli"]][[manipulation_name]] <- manipulation_stimuli
  design[["presentations"]][[manipulation_name]] <- .add_stimulus_presentation(design,
                                                                               manipulation_name,
                                                                               formula_cols,
                                                                               manipulation_stimuli,
                                                                               is_crossed,
                                                                               is_ordered)

  .set_stimulus_printmsg(design, manipulation_name, formula_cols, is_crossed)
}

.check_if_contains <- function(design, formulas){
  # Only need to check 2 sided formulas
  formulas <- formulas[vapply(formulas, length, 1L) == 3L]
  manipulations <- unlist(lapply(formulas, function(x) stringr::str_extract_all(x[[2]], "[^* ]+")[[1L]]))

  is_in_manipulations <- manipulations %in% names(design[["manipulations"]])
  if (!all(is_in_manipulations))
    stop(paste0(paste0(manipulations[!is_in_manipulations], collapse = ", "), " not found in manipulations"))
}

.set_stimuli_printmsg <- function(design) {
  new_printmsg <- "Each trial presents these stimuli:\n"
  for (i in seq_len(length(design[["stimuli"]]))) {
    i_printmsg <- attr(design[['stimuli']][[i]], 'printmsg')
    new_printmsg <- paste0(new_printmsg, i_printmsg)
  }
  attr(design[["stimuli"]], "printmsg") <- new_printmsg
  design
}

.set_stimulus_printmsg <- function(design, stimulus, to_add, is_crossed = FALSE) {
  if (stimulus == "item_constants") {
    new_printmsg <- paste0(glue::glue("  1 of {to_add}, which only varies by trial.\n",.trim = FALSE),
                           collapse = "")
  }
  else {
    stimstring <- gsub(" x ", " and ", stimulus)
    lookup <- strsplit(stimulus, " x ")[[1L]]
    which_ordered <- vapply(lookup,
                            function(x) attr(design[["manipulations"]][[x]], "has_order"),
                            TRUE)
    n <- prod(vapply(lookup[which_ordered],
                     function(x) length(design[["orderings"]][[x]]),
                     1L
    )
    )

    new_printmsg <- paste0(glue::glue("  {n} of {to_add}, which varies by {stimstring}.\n",.trim = FALSE),
                           collapse = "")
  }
  attr(design[["stimuli"]][[stimulus]], "printmsg") <- new_printmsg
  # print(glue::glue("stimulus   {new_printmsg}"))
  design
}

.create_grid <- function(design, rhs) {
  is_crossed <- grepl(" \\* ", rhs)

  if (is_crossed) {
    # Extract the crossed manipulations
    manips <- strsplit(rhs, " \\* ")[[1L]]

    # Compose an expand.grid call with all the crossed manipulations
    eg_call <- list(quote(expand.grid))
    for (i in seq_len(length(manips))) {
      eg_call[[i + 1L]] <- design[["manipulations"]][[manips[i]]]
    }
    grid <- eval(as.call(eg_call))
    names(grid) <- manips
  } else {
    grid <- setNames(as.data.frame(design[["manipulations"]][[rhs]]),rhs)
  }
  grid[["tojoin"]] <- 1L
  grid
}

.make_singleton_columns <- function(columns){
  setNames(as.list(rep(NA, length(columns))), columns)
}

