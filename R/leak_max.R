#' Leak max
#'
#' @param group a matrix of sign-flips
#' @return the max leak value
#' @keywords max leak
#' @export
#' @examples
#' leak_max(construct_oracle_subgroup(8))
#'

leak_max = function(group) {
  max(colSums(group[, -1, drop = F]))
}
