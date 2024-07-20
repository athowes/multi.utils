#' Fixing the precision prior in `R-INLA`.
#'
#' @param x Used to specify the `initial`.
#' @return A prior that can be passed to `R-INLA`.
#' @export
tau_fixed <- function(x) {
  list(prec = list(initial = log(x), fixed = TRUE))
}

#' Penalised complexity precision prior for `R-INLA`.
#'
#' @param x Used to specify the `initial`.
#' @param u Upper threshold.
#' @param alpha Probability that the standard deviation exceeds
#' the upper threshold.
#' @return A prior that can be passed to `R-INLA`.
#' @export
tau_pc <- function(x, u, alpha) {
  list(prec = list(prec = "pc.prec", param = c(u, alpha), initial = log(x)))
}
