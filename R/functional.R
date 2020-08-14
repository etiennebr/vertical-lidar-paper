# test variance equality
functional_anova <- function(g, nsim = nperm, contrasts = TRUE, curve_set = cset_trans, ...) {
  GET::graph.fanova(
    nsim = nsim,
    curve_set = curve_set,
    groups = g,
    variances = "unequal",
    test.equality = "var",
    contrasts = contrasts,
    alpha = alpha,
    ...
  )
}

functional_lm <- function(null_model, nsim = nperm, contrasts = TRUE) {
  graph.flm(
    nsim = nsim,
    formula.full = Y ~ dominant_species + age_class + crown_closure,
    formula.reduced = null_model,
    curve_sets = cset_trans,
    factors = independent_factors,
    contrasts = contrasts,
    GET.args = list(alpha = alpha)
  )
}
