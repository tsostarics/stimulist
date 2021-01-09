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
  formulas <- lapply(enexprs(...), as.character)
  n_trials <- attr(design[['trials']], 'total')
  stim_table <- select(get_stim_table(design), -id)
  print(formulas)
  for (stimset in formulas) {

    # If this stimulus doesn't vary by any manipulation ( ~ x) then just add
    # the columns with the columns in the stimulus table and add to the design
    if (length(stimset) == 2) {
      f_cols <- strsplit(as.character(stimset[[2L]]), " \\+ ")[[1L]]
      add_cols <- lmap(f_cols, function(x) setNames(list(NA), x))
      output <- cbind(data.frame(stim_table), add_cols)
      design[['stimuli']][['constant_for_all']] <- output
      design[['presentations']][['constant_for_all']] <- output
    }
    else{
      # For stimuli that vary by a manipulation (manip ~ x + y + z...)
      # Get the manipulation name, cross by number of trials, set name
      f_manip <- stimset[[2L]] # Manipulation from lhs of formula

      ## Get columns to add from rhs of formula
      f_cols <- strsplit(as.character(stimset[[3L]]), " \\+ ")[[1L]]

      # Cross manipulation conditions with number of trials
      manipulation <- design[['manipulations']][[f_manip]]
      expanded <- expand.grid(manipulation, trial = 1:n_trials)
      names(expanded)[1L] <- f_manip # expand.grid first column name is `Var1`

      # Make empty singleton columns to bind to output
      add_cols <- set_names(as.list(rep(NA, length(f_cols))), f_cols)

      output <-
        left_join(expanded, stim_table, by = 'trial') %>%
        relocate(trial, type, stimulus) %>%
        cbind(add_cols)

      # If there's an ordering, then make a presentation using the different
      # permutations of that condition. Otherwise the presentation is the same
      # as the stimulus set.
      has_order <- attr(manipulation, 'has_order')
      if (has_order)
        design[['presentations']][[f_manip]] <- .make_presentation(design,
                                                                   f_manip,
                                                                   n_trials,
                                                                   f_cols,
                                                                   stim_table)
      else
        design[['presentations']][[f_manip]] <- output

      design[['stimuli']][[f_manip]] <- output
    }
  }
  design
}

.clean_formulas <- function(formulas){
  # note: need to add a helper function that merges formulas when they
  # address the same manipulation. eg in add_stimuli_by(~one, ~two), two
  # will overwrite one rather unlike in add_stimuli_by(~one + two)
}

#' @importFrom gtools permutations
.expand_ordering <- function(design, manipulation, n){

  # Get which manipulations have orderings
  ordered_manips <-  vapply(design[['manipulations']],
                            FUN = function(x)
                              attr(x,'has_order'),
                            FUN.VALUE = TRUE)
  if (ordered_manips[manipulation]) {
    ordering <- design[['orderings']][[manipulation]]
    ordering <- mutate(ordering, set = row_number())
    trials_df <- expand.grid(trial = 1:n, set = 1:nrow(ordering))
    expanded <- left_join(trials_df, ordering, by = 'set')
  }
  else{
    expanded <- expand.grid(design[['manipulations']][[manipulation]],
                            trial = 1:n)
    names(expanded)[1L] <- manipulation
  }
  expanded
}

.make_presentation <- function(design, manipulation, n, columns, stimulus_table){
  ordering <- design[['orderings']][[manipulation]]
  order_nums <- 1:length(ordering)
  add_cols <-
    as.vector(
      sapply(columns,
             FUN = function(x) paste(x, order_nums, sep = "_")
      )
    )

  expanded <- .expand_ordering(design, manipulation, n)

  add_cols <- set_names(as.list(rep(NA, length(columns))), columns)

  output <-
    left_join(expanded, stimulus_table, by = 'trial') %>%
    relocate(trial, type, stimulus) %>%
    cbind(add_cols)

  output
}
