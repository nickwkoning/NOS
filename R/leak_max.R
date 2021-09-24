#' Leak max
#'
#' @param group a matrix of sign-flips
#' @return the max leak value
#' @keywords max leak
#' @export
#' @examples
#' leak_max(colSums(construct_oracle_subgroup(8)))
#'

leak_max = function(leak_terms) {
  max(leak_terms)
}
