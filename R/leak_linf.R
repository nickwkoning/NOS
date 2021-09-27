#' Leak l-infinity
#'
#' @param group a matrix of sign-flips
#' @return the l-infinity leak value
#' @keywords linf leak max abs
#' @export
#' @examples
#' leak_linf(colSums(construct_oracle_subgroup(8)))
#'

leak_linf = function(leak_terms) {
  max(abs(leak_terms))
}
