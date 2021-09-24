#' Construct group expansion that minimizes the leak
#'
#' @param group the group
#' @param num_elements number of candidates generated for each group expansion
#' @param leak_fun the function used to compare groups
#' @return an expanded group
#' @keywords expand group leak
#' @export
#' @examples
#' leak_minimizing_group_expansion(construct_oracle_subgroup(4), 10, leak_max)
#'


leak_minimizing_group_expansion = function(group, num_elements, leak_fun) {
  if (ncol(group) >= 2^nrow(group)) {
    return(group)
  }

  candidate_elements = construct_candidate_elements(group, num_elements)
  leak_terms_group = colSums(group[, -1, drop = F])

  # add candidate to group and close the group
  leak = c()
  for (i in 1:ncol(candidate_elements)) {
    # multiply existing elements with the candidate and concatenate to group
    group_expansion = group * candidate_elements[, i]
    leak_terms_expansion = colSums(group_expansion)
    leak[i] = leak_fun(c(leak_terms_group, leak_terms_expansion))
  }

  # select best candidate group
  min_index = which.min(leak)
  group = close_group_element(group, candidate_elements[, min_index])

  return(group)
}
