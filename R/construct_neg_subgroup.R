#' Construct a negative subgroup
#'
#' Constructs a single negative subgroup for n observations of order 2n
#' @param n number of observations that is a power of 2
#' @return a negative subgroup as a n x 2n matrix, with elements in columns
#' @keywords construct negative subgroup
#' @export
#' @examples
#' construct_neg_subgroup(construct_oracle_subgroup(4))

construct_neg_subgroup = function(group) {
  subgroup = cbind(group, -group)

  return(subgroup)
}
