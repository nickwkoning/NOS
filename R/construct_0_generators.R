#' Construct the generators for a 0-subgroup
#'
#' @param n number of observations
#' @return matrix with generators for a 0-subgroup as rows. Identity in column 1
#' @keywords generators 0-subgroup
#' @export
#' @examples
#' construct_0_generators(4)

construct_0_generators = function(n) {
  p = log2(n)
  generators = matrix(nrow = n, ncol = p + 1)

  generators[, 1] = 1

  for (i in 1:p) {
    generators[, i + 1] = as.integer(0:(n-1) %% 2^i < 2^(i-1))
  }

  generators = 2 * generators - 1

  return(generators)
}
