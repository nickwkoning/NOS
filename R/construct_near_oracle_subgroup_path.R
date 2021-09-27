#' Construct a path of near oracle subgroups
#'
#' @param n number of signs per sign-flip element
#' @param num_candidates number of candidates generated with each group expansion
#' @param max_order maximum rank (log2(order)) of the group
#' @param leak_fun the function used to compare groups
#' @param group # optional. A starting group. Default is just the identity group
#' @return a near oracle group
#' @keywords near oracle group
#' @export
#' @examples
#' construct_near_oracle_subgroup_path(4, 100, 10, leak_max)
#'



construct_near_oracle_subgroup_path = function(n, num_candidates, max_rank, leak_fun, one_sided) {
  subgroup_list = list()

  oracle_exhausted = FALSE
  counter = 1
  for (max_order in 2^(0:max_rank)) {
    if (2^n < max_order) {
      break
    }

    if (!oracle_exhausted) {
      group = construct_oracle_subgroup(n, max_order)
      if (one_sided) {
        group = construct_neg_subgroup(group)
      }
      if (ncol(group) < max_order) {
        oracle_exhausted = TRUE
      }
    }

    if (oracle_exhausted) {
      group = subgroup_list[[counter - 1]]
      subgroup_list[[counter]] = leak_minimizing_group_expansion_batched(group,
                                                                         num_candidates,
                                                                         leak_fun)
    } else {
      subgroup_list[[counter]] = group
    }

    counter = counter + 1

  }
  return(subgroup_list)
}
