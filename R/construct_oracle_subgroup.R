#' Constructs the largest possible oracle subgroup
#'
#' Constructs an oracle subgroup for n observations of the largest order smaller
#' than max_order.
#' @param n number of observations that is a power of 2
#' @param max_order maximum order of the oracle subgroup. Default is n
#' @return an oracle subgroup as an p x n matrix, with elements in columns
#' @keywords construct oracle subgroup
#' @export
#' @examples
#' construct_oracle_subgroup(4)

construct_oracle_subgroup = function(n, max_order = n) {
  if (max_order == 1) {
    return(matrix(rep(1, n), nrow = n))
  }

  oracle_generators = construct_oracle_generators(n)

  if (any(is.null(oracle_generators))) {
    return(matrix(rep(1, n), nrow = n))
  }

  p = floor(log2(max_order)) + 1
  p = min(p, ncol(oracle_generators))
  group = close_group_generators(oracle_generators[, 1:p, drop = F])

  if (ncol(group) < max_order) {
    warning(paste0("No subgroup of order ", max_order, " exists."))
  }

  return(group)
}

