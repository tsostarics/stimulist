set_filepaths <- function(design, template, name_by = 'stimulus'){
  file_options <- c('prefix','prefix_by','name_by', 'suffix_by', 'suffix')
  file_spec <- names(design[['files']])
  file_keep <- file_options %in% c(file_spec, 'name_by')

  paste_call <- quote(paste(prefix,
                            .data[[prefix_by]],
                            .data[[name_by]],
                            .data[[sufix_by]],
                            suffix,
                            sep = '_'))
  paste_call <- paste_call[c(T, file_keep)]
  with(design[['files']], mutate(template,
                                 filepath = eval(paste_call),
                                 filepath = paste0(directory, filepath, extension)))
}
