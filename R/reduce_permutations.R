reduce_permutations <- function(design, ...){
  # 0: enexprs the ..., cast to character
  # 1: check if lhs is in orderings
  # 2: convert a*b + c*b specifications into a char vector
  # 3: do design[['orderings']][[lhs]] <- chvect[vapply(design[['orderings']][[lhs]], \x !x %in% chvect, T)]
}

reduce_combinations <- function(design, ...){
  # 0: same preprocessing steps as reduce_permutations
  # 1: take the a*b pairs and expand to be b*a
  # this should be essentially a wrapper that uses
  # contextcontour ~ hllnone * lllnone
  # gtools::permutations(2, r = 2, c("hllnone","lllnone"))
  # to generate:
  # contextcontour ~ hllnone * lllnone + lllnone*hllnone
  # and pass to reduce_permutations
  # gtools::permutations(2, r = 2, c("hllnone","lllnone")) %>%
  # apply(2, function(x) paste(x, collapse = " * ")) %>%
  # paste(collapse = " + ")
}

.preprocess_formulas <- function(design, formulas, component) {

}
