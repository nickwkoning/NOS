#' Leak l1
#'
#' @param group a matrix of sign-flips
#' @return the l1 leak value
#' @keywords l1 leak
#' @export
#' @examples
#' leak_l1(construct_oracle_subgroup(8))
#'
leak_l1 = function(group) {
  mean(abs(colSums(group[, -1, drop = F])))
}
