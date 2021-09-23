#' Constructs the largest possible oracle subgroup
#'
#' Constructs an oracle subgroup for n observations of desired order,
#' given that it exists
#' @param n number of observations that is a power of 2
#' @return an oracle subgroup as an p x n matrix, with elements in columns
#' @keywords construct oracle subgroup
#' @export
#' @examples
#' construct_oracle_subgroup(4)

construct_oracle_subgroup = function(n, max_order = n) {
  oracle_generators = construct_oracle_generators(n)
  if(any(is.null(oracle_generators))) {
    return(NULL)
  }

  p = floor(log2(max_order)) + 1
  p = min(p, ncol(oracle_generators))
  group = close_group_generators(oracle_generators[, 1:p, drop = F])

  return(group)
}

