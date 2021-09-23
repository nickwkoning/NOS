#' Given a group and an element not in the group, close the group
#'
#' @param n number of signs per sign-flip element
#' @param num_candidates number of candidates generated with each group expansion
#' @param max_order maximum order of the group
#' @param leak_fun the function used to compare groups
#' @param group # optional. A starting group. Default is just the identity group
#' @return a near oracle group
#' @keywords near oracle group
#' @export
#' @examples
#' construct_near_oracle_subgroup(4, 100, 8, leak_max)
#'


construct_near_oracle_subgroup = function(n, num_candidates,
                                          max_order, leak_fun,
                                          group = NULL) {
  if (is.null(group)) {
    group = matrix(rep(1, n), nrow = n)
  }

  # Keep going until group is of desired order
  while(2 * ncol(group) <= max_order) {
    # Randomly generate candidate elements
    candidate_elements = replicate(num_candidates,
                                   sample(c(-1, 1), n, replace = TRUE))

    candidate_elements = matrix(candidate_elements, ncol = num_candidates)
    # Ensure set of candidate elements is of size num_candidates and has no
    # duplicates nor shares any elements with the group
    while (ncol(candidate_elements) < num_candidates && num_candidates + ncol(group) < 2^n) {
      candidates_list = as.list(as.data.frame(candidate_elements))
      group_list = as.list(as.data.frame(group))

      in_group = candidates_list %in% group_list
      candidate_elements = candidate_elements[, !remove, drop = F]

      duplicates = duplicated_sign_flips(candidate_elements)
      candidate_elements = candidate_elements[, -duplicates, drop = F]

      num_removed = num_candidates - ncol(candidate_elements)
      new_candidates = replicate(num_removed,
                                 sample(c(-1, 1), n, replace = TRUE))
      candidate_elements = cbind(candidate_elements, new_candidates)
    }

    # add candidate to group and close the group
    candidate_groups = list()
    for (i in 1:ncol(candidate_elements)) {
      # multiply existing elements with the candidate and concatenate to group
      candidate_groups[[i]] = close_group_element(group, candidate_elements[, i])
    }

    # select best candidate group
    min_index = which.min(sapply(candidate_groups, leak_fun))
    group = candidate_groups[[min_index]]
  }

  return(group)
}

# Ideas for tomorrow: spit out all the subgroups here
# Do not create a set of 100k candidates but batch it somehow
