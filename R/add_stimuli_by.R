#' Add Stimuli by Manipulations
#'
#' @param design Design
#' @param ... A series of formulas where the left hand side is a manipulation
#' and the right hand side is a series of variable names
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#' new_design() %>%
#' add_manipulations(context = c("IN", "OUT"), contour = c("HLL", "LLL")) %>%
#' add_trials(critical = 12, filler = 8) %>%
#' add_stimuli_by(context ~ preamble_text + question_text, contour ~ audio_file)
#'
add_stimuli_by <- function(design, ...){
  # a ~ b + c becomes c('~', 'a', 'b + c') w/ as.character
  # note that in a onesided formula it would be c('~','b + c')
  formulas <- lapply(enexprs(...), as.character)
  # n_trials <- attr(design[['trials']], 'total')

  for (stimset in formulas) {

    # If this stimulus doesn't vary by any manipulation ( ~ x) then just bind
    # the columns to the columns in the stimulus table and add to the design
    if (length(stimset) == 2) {
      f_cols <- strsplit(stimset[[2L]], " \\+ ")[[1L]]
      add_cols <- lmap(f_cols, function(x) setNames(list(NA), x))
      output <- cbind(data.frame(tojoin = 1), add_cols)
      design[['stimuli']][['constant_for_all']] <- output
      design[['presentations']][['constant_for_all']] <- output
    }
    else{
      # For stimuli that vary by a manipulation (manip ~ x + y + z...)
      # Get the manipulation name, cross by number of trials, set name
      f_manip <- stimset[[2L]] # Manipulation from lhs of formula
      is_crossed <- grepl(' \\* ', f_manip)
      ## Get columns to add from rhs of formula
      f_cols <- strsplit(as.character(stimset[[3L]]), " \\+ ")[[1L]]

      # For doing crossed manipulations
      if (is_crossed) {
        # Extract the crossed manipulations
        f_manips <- strsplit(f_manip, " \\* ")[[1]]

        # Compose an expand.grid call with all the crossed manipulations
        eg_call <- list(quote(expand.grid))
        for (i in 1:length(f_manips)) {
          eg_call[[i + 1L]] <- design[['manipulations']][[f_manips[i]]]
        }
        grid <- eval(as.call(eg_call))
        names(grid) <- c(f_manips)
        f_manip <- gsub(' \\* ', ' x ', f_manip) # change * to x for filenames
      } else{
        # Cross manipulation conditions with number of trials
        manipulation <- design[['manipulations']][[f_manip]]
        grid <- expand.grid(manipulation)
        names(grid)[1L] <- f_manip # expand.grid first column name is `Var1`
        grid$tojoin <- 1L
        grid <- dplyr::relocate(grid, tojoin)
      }
      # Make empty singleton columns to bind to output
      add_cols <- set_names(as.list(rep(NA, length(f_cols))), f_cols)

      output <- cbind(grid, add_cols)

      design[['stimuli']][[f_manip]] <- output
      print(output)

      if (!is_crossed) {
        has_order <- attr(manipulation, 'has_order')
        if (has_order)
          design[['presentations']][[f_manip]] <- .make_presentation(design,
                                                                     f_manip,
                                                                     f_cols)
        else
          design[['presentations']][[f_manip]] <- output
      } else design[['presentations']][[f_manip]] <-
        .make_crossed_presentation(design,
                                   f_manip,
                                   f_cols)
    }
  }
  design
}

.clean_formulas <- function(formulas){
  # note: need to add a helper function that merges formulas when they
  # address the same manipulation. eg in add_stimuli_by(~one, ~two), two
  # will overwrite one unlike in add_stimuli_by(~one + two)
}

.make_presentation <- function(design, manipulation, columns){
  # Extract ordering for this manipulation, and get the number to present
  ordering <- design[['orderings']][[manipulation]]
  order_nums <- 1:length(ordering)
  # For each of the columns to add, append 1:order_nums
  add_cols <-
    as.vector(
      sapply(columns,
             FUN = function(x) paste(x, order_nums, sep = "_")
      )
    )

  # Turn the vector of columns to add into a named list for cbind
  add_cols <- set_names(as.list(rep(NA, length(add_cols))), add_cols)

  output <- cbind(ordering, add_cols)
  output$tojoin <- 1
  dplyr::relocate(output, tojoin)
}

.cross_manipulations <- function(design, manipulations){
  manipulations <- strsplit(manipulations, " x ")[[1]]
  print(names(design[['orderings']]))
  is_ordered <- manipulations %in% names(design[['orderings']])
  orderings <- design[['orderings']][manipulations[is_ordered]]
  unorderings <- design[['manipulations']][manipulations[!is_ordered]]

  unordered_dfs <-
    lmap(unorderings,
         function(x){
           outdf <- as.data.frame(x)
           # names(outdf) <- names(x)
           outdf$tojoin <- 1
           list(outdf)
         })

  ordered_dfs <-
    lmap(orderings,
         function(x){
           x[[1]]$tojoin <- 1
           x
         })

  print(unordered_dfs)
  print(ordered_dfs)

  merged_ordered <-
    Reduce(function(x,y)
      merge(x,y, by = 'tojoin', all = T),
      ordered_dfs)

  merged_unordered <-
    Reduce(function(x,y)
      merge(x,y, by = 'tojoin', all = T),
      unordered_dfs)

  if (is.null(merged_ordered) & is.null(merged_unordered))
    stop("No variables provided or invalid variables")

  if (is.null(merged_ordered))
    merged_all <- merged_unordered
  else if (is.null(merged_unordered))
    merged_all <- merged_ordered
  else
    merged_all <- merge(merged_unordered, merged_ordered, by = 'tojoin', all = T)
  dplyr::relocate(merged_all, tojoin)
}

.make_crossed_presentation <- function(design, manipulations, columns){
  crossed <- .cross_manipulations(design, manipulations)
  order_nums <-
    names(crossed) %>%
    str_extract('\\d$') %>%
    as.integer() %>%
    max(na.rm = TRUE)
  if (is.infinite(order_nums))
    return(crossed)
  order_nums <- 1:order_nums

  add_cols <-
    as.vector(
      sapply(columns,
             FUN = function(x) paste(x, order_nums, sep = "_")
      )
    )
  add_cols <- set_names(as.list(rep(NA, length(add_cols))), add_cols)
  cbind(crossed, add_cols)
}
