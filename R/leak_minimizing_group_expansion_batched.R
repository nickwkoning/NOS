#' Construct group expansion that minimizes the leak using batches to reduce
#' memory usage
#'
#' @param group the group
#' @param num_elements number of candidates generated for each group expansion
#' @param leak_fun the function used to compare groups
#' @return an expanded group
#' @keywords expand group leak batched
#' @export
#' @examples
#' leak_minimizing_group_expansion_batched(construct_oracle_subgroup(4), 10, leak_max, 2)
#'
#'
leak_minimizing_group_expansion_batched = function(group,
                                                   num_elements,
                                                   leak_fun,
                                                   batch_size = min(1000, num_elements)) {
  groups = list()
  for (i in 1:(num_elements / batch_size)) {
    groups[[i]] = leak_minimizing_group_expansion(group, batch_size, leak_fun)
  }

  min_index = which.min(sapply(groups, leak_fun))
  group = groups[[min_index]]

  return(group)

}
