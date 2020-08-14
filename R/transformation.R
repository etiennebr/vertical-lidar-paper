# Transformation to equalize variances in groups
corrUnequalVar <- function(curve_set, groups, n.aver = 1L, mirror = FALSE) {
  if (!is.factor(groups)) {
    groups <- factor(groups)
  }
  x <- GET:::data_and_sim_curves(curve_set)
  # Group means, (bar{T}_j(r))
  m <- GET:::groupmeans(x, groups)
  # Sample variance over all functions, Var(T(r))
  varT <- GET:::vvar(x)
  # Variances in the groups, Var(T_j(r))
  v <- GET:::groupvar(x, groups)
  # Moving average
  if(n.aver>1){
    v <- t(apply(v, 1, GET:::maverage, n.aver=n.aver, mirror=mirror))
    varT <- GET:::maverage(varT, n.aver=n.aver, mirror=mirror)
  }
  # Take S_ij(r) = (T_ij(r) - \bar{T}_j(r)) / \sqrt( Var(T_j(r)) ) * Var(T(r)) + \bar{T}_j(r)
  for(i in 1:nrow(x)) {
    x[i,] <- (x[i,] - m[which(rownames(m) == groups[i]),]) / sqrt(v[which(rownames(v) == groups[i]),]) * sqrt(varT) + m[which(rownames(m) == groups[i]),]
  }
  curve_set$funcs <- t(x)
  curve_set
}
