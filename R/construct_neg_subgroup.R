#' Construct the negative subgroup for a given group
#'
#' @param group a sign-flipping subgroup
#' @return the negative subgroup
#' @keywords construct negative subgroup
#' @export
#' @examples
#' construct_neg_subgroup(construct_oracle_subgroup(4))

construct_neg_subgroup = function(group) {
  subgroup = cbind(group, -group)

  return(subgroup)
}
