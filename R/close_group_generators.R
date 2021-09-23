#' Construct group from generators
#'
#' @param generators sign matrix with generators in columns
#' @param identity_index integer indicating the index of the identity element
#' @return the closed group
#' @keywords combine generators
#' @export
#' @examples
#' close_group_generators(construct_0_generators(8), 1)
#'

close_group_generators = function(generators, identity_index = NULL) {

  n = nrow(generators)
  p = log2(n)

  # removes the identity element
  if (!is.null(identity_index)) {
    generators = generators[,-identity_index]
  }

  group = matrix(nrow = n, ncol = n)
  group[, 1] = 1
  group[, 2:(p + 1)] = generators

  counter = p + 2
  for (i in 2:p) {
    comb_mat = combn(p, i) + 1
    for (ii in 1:ncol(comb_mat)) {
      comb = comb_mat[, ii]
      group[, counter] =  apply(group[,comb], 1, prod)
      counter = counter + 1
    }
  }

  return(group)
}
