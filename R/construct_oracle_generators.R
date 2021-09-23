#' Construct the generators for an oracle subgroup
#'
#' @param n number of observations
#' @return matrix with generators for an oracle subgroup as rows. Identity in column 1
#' @keywords generators oracle subgroup
#' @export
#' @examples
#' construct_oracle_generators(4)

construct_oracle_generators = function(n) {
  p_max = floor(log2(n)) + 1
  for (p in 1:p_max) {
    if (n %% 2^p > 0) {
      break
    }
  }
  p = p - 1

  if (p == 0) {
    return(NULL)
  }
  generators = matrix(nrow = n, ncol = p + 1)

  generators[, 1] = 1

  for (i in 1:p) {
    generators[, i + 1] = as.integer(0:(n-1) %% 2^i < 2^(i-1))
  }

  generators = 2 * generators - 1

  return(generators)
}

