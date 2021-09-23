## Random sign flips

random_signs = function(n, N, refl = NULL) {

  if (log2(n) <= 4 & is.null(refl)) {
    refl = generate_sign_flipping_group(n)
  }

  W = matrix(nrow = n, ncol = N)

  if(log2(n) <= 4) {
    subset = sample(2:ncol(refl), N - 1)
    W[, 1] = 1
    W[, 2:N] = refl[, subset]
  } else {
    refl_sample = list()
    refl_sample[[1]] = rep(1, n)
    len_sample = 1
    while (len_sample < N) {
      for (ii in 1:(N - len_sample)) {
        refl_sample[[len_sample + 1]] = sample(c(-1, 1), n, replace = TRUE)
        len_sample = len_sample + 1
      }
      refl_sample = unique(refl_sample)
      len_sample = length(refl_sample)
    }
    for (i in 1:len_sample) {
      W[, i] = refl_sample[[i]]
    }
  }
  return(W)
}
