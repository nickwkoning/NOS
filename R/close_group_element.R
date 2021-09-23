#' Given a group and an element not in the group, close the group
#'
#' @param group sign matrix with group elements in columns
#' @param new_element n-vector with signs that is not in the group
#' @return the closed group
#' @keywords close group element
#' @export
#' @examples
#' close_group_element(matrix(c(1, 1, 1, -1)), c(-1, -1))
#'

close_group_element = function(group, new_element) {
  return(cbind(group, group * new_element))
}
