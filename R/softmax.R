#' Compute the softmax of a vector.
#'
#' @param x A vector.
#' @return The softmax of `x`
#' @export
softmax <- function(x) {
  exp(x) / sum(exp(x))
}

#' Compute the numerically-stable softmax of a vector.
#'
#' @param x A vector.
#' @return The softmax of `x`
#' @export
stable_softmax <- function(x) {
  x <- x - max(x)
  exp(x) / sum(exp(x))
}
