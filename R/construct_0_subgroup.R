#' Construct a 0-subgroup
#'
#' Constructs a single 0-subgroup for n observations
#' @param n number of observations that is a power of 2
#' @return a 0-subgroup as an n x n matrix, with elements in columns
#' @keywords construct 0-subgroup
#' @export
#' @examples
#' construct_0_subgroup(4)

construct_0_subgroup = function(n) {
  p = log2(n)

  if (p %% 1 != 0) {
    stop("0-subgroup does not exist if n is not a power of 2.")
  }

  group = close_group_generators(construct_0_generators(n))

  return(group)
}

