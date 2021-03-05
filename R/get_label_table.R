#' Get table of item labels
#'
#' This is used to create a table of labels and their respective items to left
#' join in to the final table
#'
#' @param design
#'
#' @export
#'
#' @examples
.get_label_table <- function(design) {
  labelled_items <- vapply(term$items, function(x) attr(x, 'labelled'), TRUE)

  rbindlist(
    purrr::lmap(design[['items']][labelled_items],
                function(x)
                  list(
                    data.frame(item = seq_len(x[[1L]]),
                               item_label = attr(x[[1L]], "labels"),
                               type = names(x)
                    )
                  )
    )
  )

}

.any_labels <- function(design){
  any(vapply(term$items, function(x) attr(x, 'labelled'), TRUE))
}
