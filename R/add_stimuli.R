#' Add stimuli that don't vary by manipulation
#'
#' add_stimuli(stim) will do the same thing as add_stimuli_by(~stim). If you
#' have a variable such as stim <- c('one','two') you should pass this as
#' add_stimuli(!!stim). You may use add_stimuli(c('one','two')), but this
#' requires a few more key strokes than add_stimuli(one, two).
#'
#' @param design
#' @param ... Series of unquoted stimuli. If you want to pass a (character
#' vector) variable, you should unquote it with !!.
#'
#' @return
#' @export
#'
#' @examples
#'
#' # All acceptable ways to add audio and image variables (names arbitrary)
#' test_design <- new_design() %>% add_items(critical = 2, filler=1)
#' stims <- c('audio','image')
#' test_design %>% add_stimuli(audio, image)
#' test_design %>% add_stimuli('audio', 'image')
#' test_design %>% add_stimuli(c('audio', 'image'))
#' test_design %>% add_stimuli('audio + image')
#' test_design %>% add_stimuli(!!stims)
#' test_design %>% add_stimuli_by(~audio + image)
#'
add_stimuli <- function(design, ...){
  stims <- as.character(enexprs(...))
  are_vecs <- grepl('c\\(\"', stims)
  # Handle any defused character vectors
  if (any(are_vecs)) {
    stims[are_vecs] <- strsplit(gsub("c\\(\"|\"|,|\\)",
                                     "",
                                     stims[are_vecs]),
                                split = ' ')
    stims <- unlist(stims)
  }
  # Construct a formula to pass to add_stimuli_by
  stim_formula <- paste0('~', paste(stims, collapse = ' + '))
  stim_formula <- str2lang(stim_formula)
  add_stimuli_by(design,  !!stim_formula)
}
