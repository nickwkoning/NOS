#' Duplicated sign-flips
#'
#' For a given matrix with sign-flip columns, this returns the indices
#' of the duplicate columns
#'
#' @param sign_matrix a matrix of sign-flips
#' @return duplicate elements
#' @keywords duplicate sign-flip
#' @export
#' @examples
#' duplicated_sign_flips(matrix(c(1, 1, 1, -1, 1, -1), ncol = 3))
#'

duplicated_sign_flips = function(sign_matrix) {

  # identify duplicates
  duplicates = duplicated(as.list(as.data.frame(sign_matrix)))

  return(duplicates)
}

