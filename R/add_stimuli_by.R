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
  for (stimset in formulas) {
    if (length(stimset) == 2) {
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

.set_stimuli_printmsg <- function(design) {
  new_printmsg <- "Each trial presents these stimuli:"
  for (i in seq_len(length(design[["stimuli"]]))) {
    new_printmsg <- glue::glue("{new_printmsg}\n{attr(design[['stimuli']][[i]], 'printmsg')}")
  }
  attr(design[["stimuli"]], "printmsg") <- new_printmsg
  design
}

.set_stimulus_printmsg <- function(design, stimulus, to_add, is_crossed = FALSE) {
  if (stimulus == "item_constants") {
    new_printmsg <- glue::glue("  1 of {to_add}, which only varies by trial.\n")
  }
  else {
    # if (is_crossed) {
    stimstring <- gsub(" x ", " and ", stimulus)
    # } else {
    # stimstring <- stimulus
    # }

    lookup <- strsplit(stimulus, " x ")[[1]]
    which_ordered <- vapply(lookup,
                            function(x) attr(design[["manipulations"]][[x]], "has_order"),
                            TRUE)
    n <- prod(vapply(lookup[which_ordered],
                     function(x) length(design[["orderings"]][[x]]),
                     1L
    )
    )

    new_printmsg <- glue::glue("\t{n} of {to_add}, which varies by {stimstring}.\n")
  }
  attr(design[["stimuli"]][[stimulus]], "printmsg") <- new_printmsg
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

.combine_onesided_formulas <- function(formulas, which_no_lhs) {
  if (!any(which_no_lhs)) return(list())
  formula_strings <- vapply(formulas[which_no_lhs], function(x) x[[2]], "char")
  list(c("~",paste0(formula_strings,collapse = " + ")))
}

.combine_twosided_formulas <- function(formulas, which_have_lhs){
  lhs <- unique(vapply(formulas[which_have_lhs], function(x) x[[2]], "char"))
  # For each lhs, combine into one formula
  purrr::lmap(
    lhs,
    function(x) {
      # LHS given in 2nd element, RHS in 3rd if twosided
      rhs <- vapply(formulas,
                    function(y) ifelse(y[[2]] == x, y[[3]], character(0)),
                    "char"
      )
      rhs <- rhs[!is.na(rhs)]
      list(c("~", x, paste0(rhs, collapse = " + ")))
    }
  )
}

.clean_formulas <- function(formulas) {
  formula_lengths <- vapply(formulas, length, 1L)
  if (any(formula_lengths > 3 | formula_lengths < 2)) {
    stop("Malformed formula")
  }

  no_lhs <- vapply(formulas, function(x) length(x) == 2, T)
  combined_onesided <- .combine_onesided_formulas(formulas, no_lhs)
  combined_twosided <- .combine_twosided_formulas(formulas, !no_lhs)

  c(combined_onesided, combined_twosided)
}

.make_presentation <- function(design, manipulation, columns) {
  # Extract ordering for this manipulation, and get the number to present
  ordering <- design[["orderings"]][[manipulation]]
  order_nums <- seq_len(length(ordering))
  # For each of the columns to add, append 1:order_nums
  add_cols <- unlist(lapply(columns,FUN = function(x) paste(x, order_nums, sep = "_")))

  # Turn the vector of columns to add into a named list for cbind
  add_cols <- .make_singleton_columns(add_cols)

  output <- cbind(ordering, add_cols)
  output[["tojoin"]] <- 1
  output
}

.cross_manipulations <- function(design, manipulations) {
  manipulations <- strsplit(manipulations, " x ")[[1]]
  is_ordered <- manipulations %in% names(design[["orderings"]])
  ordered_manipulations <- design[["orderings"]][manipulations[is_ordered]]
  unordered_manipulations <- design[["manipulations"]][manipulations[!is_ordered]]

  unordered_dfs <-
    purrr::lmap(
      unordered_manipulations,
      function(x) {
        outdf <- as.data.frame(x)
        outdf[["tojoin"]] <- 1
        list(outdf)
      }
    )

  ordered_dfs <-
    purrr::lmap(
      ordered_manipulations,
      function(x) {
        x[[1]][["tojoin"]] <- 1
        x
      }
    )


  merged_ordered <- Reduce(function(x, y) merge(x, y, by = "tojoin", all = T), ordered_dfs)
  merged_unordered <- Reduce(function(x, y) merge(x, y, by = "tojoin", all = T), unordered_dfs)

  NO_ORDERED <- is.null(merged_ordered)
  NO_UNORDERED <- is.null(merged_unordered)

  if (NO_ORDERED & NO_UNORDERED)
    stop("No variables provided or invalid variables")

  if (NO_ORDERED) return(merged_unordered)
  if (NO_UNORDERED) return(merged_ordered)

  merge(merged_unordered, merged_ordered, by = "tojoin", all = T)
}

.make_crossed_presentation <- function(design, manipulations, columns) {
  crossed_manipulations <- .cross_manipulations(design, manipulations)
  order_nums <- as.integer(str_extract(names(crossed_manipulations), "\\d$"))
  none_specified <- all(is.na(order_nums))

  if (!none_specified) {
    order_nums <- seq_len(max(order_nums, na.rm = T))
    numbered_cols <- unlist(lapply(columns, FUN = function(x) paste(x, order_nums, sep = "_")))
    add_cols <- .make_singleton_columns(numbered_cols)
  }
  else {
    add_cols <- .make_singleton_columns(columns)
  }
  cbind(crossed_manipulations, add_cols)
}

.make_singleton_columns <- function(columns){
  setNames(as.list(rep(NA, length(columns))), columns)
}


.add_stimulus_presentation <- function(design,
                                       formula_manipulation,
                                       formula_columns,
                                       manipulation_stimuli,
                                       is_crossed,
                                       is_ordered) {
  if (is_crossed)
    return(.make_crossed_presentation(design, formula_manipulation, formula_columns))
  if (is_ordered)
    return(.make_presentation(design, formula_manipulation, formula_columns))
  return(manipulation_stimuli)
}
