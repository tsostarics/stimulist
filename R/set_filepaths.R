#' Set filepaths on template
#'
#' @param design
#' @param template
#' @param name_by
#'
#' @return
#' @export
set_filepaths <- function(design, template, name_by = 'stimulus'){
  if (!'files' %in% names(design)) return(template)

  # Determine which path features are specified
  file_options <- c('prefix','prefix_by','name_by', 'suffix_by', 'suffix')
  file_spec <- names(design[['files']])
  file_keep <- file_options %in% c(file_spec, 'name_by')

  # Create a paste call, exclude any feeatures that weren't specified
  paste_call <- quote(paste(prefix,
                            design[[prefix_by]],
                            design[[name_by]],
                            design[[sufix_by]],
                            suffix,
                            sep = '_'))
  paste_call <- paste_call[c(T, file_keep)]
  # Create filepaths, a second paste0 is used to prevent unwanted separators
  with(design[['files']], mutate(template,
                                 filepath = eval(paste_call),
                                 filepath = paste0(directory, filepath, extension)))
}
