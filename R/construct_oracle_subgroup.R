#' Constructs the largest possible oracle subgroup
#'
#' Constructs a maximum order oracle subgroup for n observations
#' @param n number of observations that is a power of 2
#' @return an oracle subgroup as an p x n matrix, with elements in columns
#' @keywords construct oracle subgroup
#' @export
#' @examples
#' construct_oracle_subgroup(4)

construct_oracle_subgroup = function(n) {
  if(any(is.null(construct_oracle_generators(n)))) {
    return(NULL)
  }

  group = close_group_generators(construct_oracle_generators(n))

  return(group)
}

