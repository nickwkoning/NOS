#' Construct group from generators
#'
#' @param generators sign matrix with generators in columns
#' @param identity_index integer indicating the index of the identity element
#' @return the closed group
#' @keywords combine generators
#' @export
#' @examples
#' close_group_generators(construct_oracle_generators(8))
#'

close_group_generators = function(generators) {
  n = nrow(generators)
  p = log2(n)

  group = matrix(rep(1, n), ncol = 1)

  for (i in 2:ncol(generators)) {
    group = close_group_element(group, generators[, i])
  }

  return(group)
}
