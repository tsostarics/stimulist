#' Add Stimuli to Design
#'
#' @param design
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_stims <- function(design,
                      ...){
  # note: this should be generalized such that stimuli is a list of different
  # lists of stimuli. this way you can model having multiple stimuli/inputs
  # on a single trial and specify the names. they should all be the same length.
  # as a sample, our design should be:
  # d[['stimuli']] = list(audio_stims = list(critical= ..., filler= ...))
  #                       hll = list(critical= ..., filler= ...),
  #                       lll = list(critical= ..., filler= ...))

  stims <- enquos(...)
  new_stims <- length(design[['stimuli']]) + 1L
  # If using placeholders
  if (length(stims) == 0) {
    design[['stimuli']][[new_stims]] <-
      lmap(design[['items']],
           function(x) setNames(list(1:x[[1L]]), names(x)))# starts each stim group at 1, might be okay though, we'll see
  #set name of [['stimuli']][[new_stims]] to stimulus, suffix with _length of stimuli
    attr(design[['stimuli']], 'placeholder') <- TRUE
    return(design)
  }

  num_stims <- sum(vapply(list(...), length, FUN.VALUE = 1L))
  num_items <- attr(design$items, 'total')

  names_stims <- names(stims)
  names_items <- names(design[['items']])

  if (all(length(design[['items']]) != 0)) {
    if (any(names_stims != names_items)){
      if (all(sort(names_stims) == sort(names_items)))
        warning("Stimuli names are not in the same order of the trial names, expect reordering")
      else
        stop("Stimuli names must be the same as the trial names")
    }
    else if (num_stims != num_items)
      stop("Number of provided stimuli must equal number of specified items")
  }
  design[['stimuli']][[new_stims]] <- list(...)
  design[['items']] <-
    lmap(design[['stimuli']],
         function(x) setNames(list(length(x[[1]])), names(x)))
  attr(design$stimuli, 'placeholder') <- FALSE
  attr(design$items, 'total') <- num_stims
  design
}
