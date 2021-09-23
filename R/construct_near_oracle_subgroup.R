

construct_near_oracle_subgroup = function(n, num_candidates, max_order, leak_fun) {
  group = matrix(rep(1, n), nrow = n)

  # Keep going until group is of desired order
  while(2 * ncol(group) <= max_order) {
    # Randomly generate candidate elements
    candidate_elements = replicate(num_candidates, sample(c(-1, 1), n, replace = TRUE))

    # Ensure set of candidate elements is of size num_candidates and has no duplicates
    # nor shares any elements with the group
    while (ncol(candidate_elements) < num_candidates) {
      remove = as.list(as.data.frame(candidate_elements)) %in% as.list(as.data.frame(group))
      candidate_elements = candidate_elements[, !remove, drop = F]
      candidate_elements = candidate_elements[, !duplicated(as.list(as.data.frame(candidate_elements))), drop = F]
      candidate_elements = cbind(candidate_elements, replicate(num_candidates - ncol(candidate_elements), sample(c(-1, 1), n, replace = TRUE)))
    }

    # add candidate to group and close the group
    candidate_groups = list()
    for (i in 1:ncol(candidate_elements)) {
      # multiply all existing elements with the candidate and concatenate to group
      candidate_groups[[i]] = cbind(group, group * candidate_elements[, i])
    }

    # select best candidate group
    min_index = sapply(candidate_groups, leak_fun) %>% which.min
    group = candidate_groups[[min_index]]
  }

  return(group)
}
