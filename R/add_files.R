add_files <- function(.data,
                      directory = './',
                      prefix = NA,
                      prefix_by = NA,
                      suffix = NA,
                      sufix_by = NA,
                      extension = ''){
  if (attr(.data$stimuli, 'placeholder'))
    message("Placeholder stimuli values are in use, be careful using these filepaths")

  listcall <- match.call()
  listcall[[1L]] <- quote(list)
  listcall[[2L]] <- NULL

  .data[['files']] <- eval(listcall)
  .data
}
