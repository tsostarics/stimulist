cross_manipulations <- function(design, ...) {
  # where ... is a series of formulas of the form
  # newname ~ manip1 * manip2

  # 0: parse out manipulation names
  # 1: verify names are in the design
  # 2: expand.grid the manipulations, then collapse paste the cols together
  # 3: create new manipulation with the name specified by the rhs
  # 4: remove old manipulations?
}
