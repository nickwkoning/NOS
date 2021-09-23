#' Construct a negative subgroup
#'
#' @param group a sign-flipping group
#' @return its negative subgroup
#' @keywords construct negative subgroup
#' @export
#' @examples
#' construct_neg_subgroup(construct_oracle_subgroup(4))

construct_neg_subgroup = function(group) {
  subgroup = cbind(group, -group)

  return(subgroup)
}
