#' Add Stimuli by Manipulations
#'
#' @param .data Design
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
add_stimuli_by <- function(.data, ...){
  # a ~ b + c becomes c('~', 'a', 'b + c') w/ as.character
  forms <- lapply(enexprs(...), as.character)

  skeleton_list <-
    lmap(forms,
         function(x){
           x <- x[[1L]]
           trial_n <- attr(.data[['trials']], 'total')
           stim_table <- select(get_stim_table(.data), -id)

           # If this stimulus doesn't vary by any manipulation ( ~ x)
           # (eg for a preamble that always appears)
           if (length(x) == 2) {
             add_cols <- strsplit(as.character(x[[2L]]), " \\+ ")[[1L]]
             add_cols <- lmap(add_cols, function(x) setNames(list(NA), x))
             output <- list(cbind(data.frame(stim_table), add_cols))

             setNames(output, 'constant_for_all')
           }
           else{
             # For stimuli that vary by a manipulation (manip ~ x + y + z...)
             ## Get the manipulation name, cross by number of trials, set name
             f_manip <- x[[2L]] # Manipulation from lhs of formula
             expanded <- expand.grid(.data[['manipulations']][[f_manip]],
                                     trial = 1:trial_n)
             names(expanded)[1L] <- f_manip

             ## Get columns to add from rhs of formula
             add_cols <- strsplit(as.character(x[[3L]]), " \\+ ")[[1L]]
             add_cols <- lmap(add_cols, function(x) setNames(list(NA), x))

             output <-
               left_join(expanded, stim_table) %>%
               relocate(trial, type, stimulus) %>%
               cbind(add_cols)

             setNames(list(output), f_manip)
           }
         }
    )
  # Add stimulus skeletons to our design
  .data[['stimuli']] <- skeleton_list
  .data
}
