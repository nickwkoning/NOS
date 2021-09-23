# leak l1

leak_l1 = function(group) {
  mean(abs(colSums(group[, -1, drop = F])))
}
