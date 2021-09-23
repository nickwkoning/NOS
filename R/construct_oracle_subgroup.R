#' Construct an oracle subgroup
#'
#' Constructs a single oracle subgroup for n observations
#' @param n number of observations that is a power of 2
#' @return an oracle subgroup as an n x n matrix, with elements in columns
#' @keywords construct oracle subgroup
#' @export
#' @examples
#' construct_oracle_subgroup(4)

construct_oracle_subgroup = function(n) {
  p = log2(n)

  if (p %% 1 != 0) {
    stop("oracle subgroup does not exist if n is not a power of 2.")
  }

  group = close_group_generators(construct_oracle_generators(n))

  return(group)
}

