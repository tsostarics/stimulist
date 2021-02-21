#' Add Stimuli by Manipulations
#'
#' @param design Design
#' @param ... A series of formulas where the left hand side is a manipulation
#' and the right hand side is a series of variable names
#'
#'
#' @return
#' @export
#' @importFrom stats setNames
add_stimuli_by <- function(design, ...) {
  formulas <- .clean_formulas(lapply(enexprs(...), as.character))

  for (stimset in formulas) {

    # If this stimulus doesn't vary by any manipulation ( ~ x)
    if (length(stimset) == 2) {
      f_cols <- strsplit(stimset[[2L]], " \\+ ")[[1L]]
      add_cols <- purrr::lmap(f_cols, function(x) setNames(list(NA), x))
      output <- cbind(data.frame(tojoin = 1), add_cols)
      design[["stimuli"]][["constant_for_all"]] <- output
      design[["presentations"]][["constant_for_all"]] <- output
      design <- .set_stimulus_printmsg(design, "constant_for_all", f_cols)
    }
    else {
      # For stimuli that vary by a manipulation (manip ~ x + y + z...)
      # Get the manipulation name, cross by number of items, set name
      f_manip <- stimset[[2L]] # Manipulation from lhs of formula
      is_crossed <- grepl(" \\* ", f_manip)
      f_cols <- strsplit(as.character(stimset[[3L]]), " \\+ ")[[1L]]

      # Create grid for the manipulation
      grid <- .create_grid(design, f_manip)

      f_manip <- gsub(" \\* ", " x ", f_manip) # change * to x for filenames

      # Make empty singleton columns to bind to output
      add_cols <- setNames(as.list(rep(NA, length(f_cols))), f_cols)

      output <- cbind(grid, add_cols)

      design[["stimuli"]][[f_manip]] <- output

      if (is_crossed) {
        design[["presentations"]][[f_manip]] <-
          .make_crossed_presentation(
            design,
            f_manip,
            f_cols
          )
      } else {
        if (attr(design[["manipulations"]][[f_manip]], "has_order")) {
          design[["presentations"]][[f_manip]] <- .make_presentation(
            design,
            f_manip,
            f_cols
          )
        } else {
          design[["presentations"]][[f_manip]] <- output
        }
      }
      design <- .set_stimulus_printmsg(design, f_manip, f_cols, is_crossed)
    }
  }
  .set_stimuli_printmsg(design)
}

.set_stimuli_printmsg <- function(design) {
  new_printmsg <- "Each trial presents these stimuli:\n"
  for (i in 1:length(design[['stimuli']])) {
    new_printmsg <- paste0(new_printmsg, attr(design[['stimuli']][[i]], "printmsg"))
  }
  attr(design$stimuli, "printmsg") <- new_printmsg
  design
}

.set_stimulus_printmsg <- function(design, stimulus, to_add, is_crossed = FALSE) {
  if (stimulus == "constant_for_all") {
    new_printmsg <- paste0("  1 of ",
                           to_add,
                           ", which only varies by trial.\n",
                           collapse = ""
    )
  }
  else {
    if (is_crossed) {
      stimstring <- gsub(" x ", " and ", stimulus)
    } else {
      stimstring <- stimulus
    }

    lookup <- strsplit(stimulus, " x ")[[1]]
    is_ordered <-
      vapply(
        lookup,
        function(x) {
          attr(design[["manipulations"]][[x]], "has_order")
        },
        TRUE
      )
    n <-
      prod(
        vapply(
          lookup[is_ordered],
          function(x) length(design[["orderings"]][[x]]),
          1L
        )
      )
    new_printmsg <- paste0("  ",
                           n,
                           " of ",
                           to_add,
                           ", which varies by ",
                           stimstring,
                           ".\n",
                           collapse = ""
    )
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
    for (i in 1:length(manips)) {
      eg_call[[i + 1L]] <- design[["manipulations"]][[manips[i]]]
    }
    grid <- eval(as.call(eg_call))
    names(grid) <- manips
  } else {
    grid <-
      setNames(
        as.data.frame(design[["manipulations"]][[rhs]]),
        rhs
      )
  }
  grid$tojoin <- 1L
  grid
}

.clean_formulas <- function(formulas) {
  formula_lengths <- vapply(formulas, length, 1L)
  if (any(formula_lengths > 3 | formula_lengths < 2))
    stop("Malformed formula")

  # Handle one sided formulas
  no_lhs <- vapply(tst, function(x) length(x) == 2, T)
  combined_onesided <-
    c("~",
      paste0(
        vapply(
          formulas[no_lhs],
          function(x) x[[2]], 'char'
        ),
        collapse = ' + '
      )
    )

  # Handle two sided formulas
  # Get all the variables given in the lhs of the formulas
  lhs <-  unique(vapply(formulas[!no_lhs], function(x) x[[2]], 'char'))
  combined_twosided <-
    # For each lhs variable, combine into one formula
    purrr::lmap(lhs,
                function(x){
                  rhs <- vapply(formulas,
                                function(y)
                                  ifelse(y[[2]] == x, # LHS given in 2nd element
                                         y[[3]], # RHS given in 3rd element
                                         character(0)
                                  ),
                                "char"
                  )
                  rhs <- rhs[!is.na(rhs)]
                  list(c("~", x, paste0(rhs, collapse = " + ")))
                }
    )

  c(list(combined_onesided), combined_twosided)

}

.make_presentation <- function(design, manipulation, columns) {
  # Extract ordering for this manipulation, and get the number to present
  ordering <- design[["orderings"]][[manipulation]]
  order_nums <- 1:length(ordering)
  # For each of the columns to add, append 1:order_nums
  add_cols <-
    as.vector(
      sapply(columns,
             FUN = function(x) paste(x, order_nums, sep = "_")
      )
    )

  # Turn the vector of columns to add into a named list for cbind
  add_cols <- setNames(as.list(rep(NA, length(add_cols))), add_cols)

  output <- cbind(ordering, add_cols)
  output$tojoin <- 1
  output
}

.cross_manipulations <- function(design, manipulations) {
  manipulations <- strsplit(manipulations, " x ")[[1]]
  is_ordered <- manipulations %in% names(design[["orderings"]])
  orderings <- design[["orderings"]][manipulations[is_ordered]]
  unorderings <- design[["manipulations"]][manipulations[!is_ordered]]

  unordered_dfs <-
    purrr::lmap(
      unorderings,
      function(x) {
        outdf <- as.data.frame(x)
        outdf$tojoin <- 1
        list(outdf)
      }
    )

  ordered_dfs <-
    purrr::lmap(
      orderings,
      function(x) {
        x[[1]][['tojoin']] <- 1
        x
      }
    )


  merged_ordered <-
    Reduce(
      function(x, y) {
        merge(x, y, by = "tojoin", all = T)
      },
      ordered_dfs
    )

  merged_unordered <-
    Reduce(
      function(x, y) {
        merge(x, y, by = "tojoin", all = T)
      },
      unordered_dfs
    )

  if (is.null(merged_ordered) & is.null(merged_unordered)) {
    stop("No variables provided or invalid variables")
  }

  if (is.null(merged_ordered)) {
    merged_all <- merged_unordered
  } else if (is.null(merged_unordered)) {
    merged_all <- merged_ordered
  } else {
    merged_all <- merge(merged_unordered, merged_ordered, by = "tojoin", all = T)
  }
  merged_all
}

.make_crossed_presentation <- function(design, manipulations, columns) {
  crossed <- .cross_manipulations(design, manipulations)
  order_nums <- as.integer(str_extract(names(crossed), "\\d$"))
  none_specified <- all(is.na(order_nums))

  if (!none_specified) {
    order_nums <- 1:max(order_nums, na.rm = T)
    numbered_cols <-
      as.vector(
        sapply(columns,
               FUN = function(x) paste(x, order_nums, sep = "_")
        )
      )
    add_cols <- setNames(as.list(rep(NA, length(numbered_cols))), numbered_cols)
  }
  else {
    add_cols <- setNames(as.list(rep(NA, length(columns))), columns)
  }
  cbind(crossed, add_cols)
}
