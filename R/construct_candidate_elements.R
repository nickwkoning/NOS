#' Construct potential elements that could be added to the group
#'
#' @param group the group
#' @param num_candidates number of candidates generated for each group expansion
#' @param leak_fun the function used to compare groups
#' @return a set of candidate elements
#' @keywords candidate elements
#' @export
#' @examples
#' construct_candidate_elements(construct_oracle_subgroup(4), 10)
#'


construct_candidate_elements = function(group, num_candidates) {
  n = nrow(group)
  num_candidates = min(2^n - ncol(group), num_candidates)

  candidate_elements = matrix(sample(c(-1, 1),
                                     n * num_candidates,
                                     replace = TRUE),
                              nrow = n)

  candidate_elements = remove_unwanted_elements(candidate_elements, group)

  # Ensure set of candidate elements is of size num_candidates and has no
  # duplicates nor shares any elements with the group
  while (ncol(candidate_elements) < num_candidates) {
    num_removed = num_candidates - ncol(candidate_elements)
    new_candidates = matrix(sample(c(-1, 1),
                                   n * max(num_removed, 10),
                                   replace = TRUE),
                            nrow = n)

    candidate_elements = cbind(candidate_elements, new_candidates)
    candidate_elements = remove_unwanted_elements(candidate_elements, group)
  }

  return(candidate_elements[, 1:num_candidates])
}
