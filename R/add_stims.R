add_stims <- function(.data,
                      ...){
  stims <- enquos(...)

  if (length(stims) == 0) { # Placeholder names
    .data[['stimuli']] <-
      lmap(.data[['trials']],
           function(x) setNames(list(1:x[[1L]]), names(x)))
    attr(.data[['stimuli']], 'placeholder') <- TRUE
    return(.data)
  }

  num_stims <- sum(vapply(list(...), length, FUN.VALUE = 1L))
  num_trials <- attr(.data$trials, 'total')

  names_stims <- names(stims)
  names_trials <- names(.data[['trials']])

  if (all(length(.data[['trials']]) != 0)) {
    if (any(names_stims != names_trials))
      if (any(sort(names_stims) == sort(names_trials)))
        warning("Stimuli names are not in the same order of the trial names, expect reordering")
    else
      stop("Stimuli names must be the same as the trial names")
    else if (num_stims != num_trials)
      stop("Number of provided stimuli must equal number of specified trials")
  }
  .data[['stimuli']] <- list(...)
  .data[['trials']] <-
    lmap(.data[['stimuli']],
         function(x) setNames(list(length(x[[1]])), names(x)))
  attr(.data$stimuli, 'placeholder') <- FALSE
  attr(.data$trials, 'total') <- num_stims
  .data
}
