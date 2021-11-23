#' Leak mean
#'
#' @param leak_terms a vector of a sum of sign-flips
#' @return the mean leak value
#' @keywords mean leak
#' @export
#' @examples
#' leak_mean(colSums(construct_oracle_subgroup(8)))
#'

leak_mean = function(leak_terms) {
  mean(leak_terms)
}
