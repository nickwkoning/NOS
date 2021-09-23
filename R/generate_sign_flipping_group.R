#' Generate the sign-flipping group
#'
#' @param n number of observations
#' @return the sign-flipping group in dimension n
#' @keywords sign-flipping group
#' @export
#' @examples
#' generate_sign_flipping_group(8)
#'

generate_sign_flipping_group = function(n) {
  if (n > 20 && n <= 30) {
    warning("Generating the sign-flipping group for n > 20 may be very slow.")
  } else if (n > 30) {
    stop("Generating the sign-flipping group for n > 30 is not supported.")
  }

  group = matrix(nrow = n, ncol = 2^n)
  for (i in 1:2^n) {
    element = 2 * as.numeric(intToBits(i - 2))[1:n] - 1
    group[, i] = element
  }

  return(group)
}
