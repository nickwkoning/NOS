# leak max

leak_max = function(group) {
  max(colSums(group[, -1, drop = F]))
}
