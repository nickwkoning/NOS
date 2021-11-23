#' Removes unwanted elements from the set of sign-flips
#'
#' @param elements a matrix of sign-flips as columns
#' @param group a group which the elements should not be in
#' @return a set of elements with the unwanted elements removed
#' @keywords remove unwanted elements sign-flip
#' @export
#'

remove_unwanted_elements = function(elements, group) {
  new_set = cbind(group, elements)

  duplicated = duplicated_sign_flips(new_set)
  new_set = new_set[, !duplicated, drop = F]
  elements = new_set[, -(1:ncol(group)), drop = F]

  return(elements)
}
