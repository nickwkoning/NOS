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
  while (2 * ncol(group) <= max_order && ncol(group) < 2^n) {
    group = leak_minimizing_group_expansion_batched(group, num_candidates,
                                                    leak_fun)
  }

  return(group)
}

# Ideas for tomorrow: spit out all the subgroups here
