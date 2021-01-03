#' Add filepath information
#'
#' @param .data Experiment Design
#' @param directory Directory the files will be held in
#' @param prefix Prefix to give all files
#' @param prefix_by Prefix by a particular condition
#' @param suffix Suffix to give all files
#' @param sufix_by Suffix by a particular condition
#' @param extension File extension of files
#'
#' @return
#' @export
#'
#' @examples
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
