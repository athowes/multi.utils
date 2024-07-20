#' Repeat a matrix along the diagonal.
#'
#' The output should have dimensions `dim(M) * n`.
#'
#' @param M A matrix.
#' @param n The number of times to be repeated.
#' @return A repeated matrix.
#' @export
repeat_matrix <- function(M, n) {
  MM <- Matrix::bdiag(rep(list(M), n))
  rownames(MM) <- 1:nrow(MM)
  colnames(MM) <- 1:ncol(MM)
  return(MM)
}

#' Categorical to indicators.
#'
#' @param x A categorical column.
#' @return Indicator variables column.
#' @export
to_int <- function(x) {
  as.integer(as.factor(x))
}

#' Mutate a new set of columns for dummy variable coding of `var`.
#'
#' The `var` column is a categorical variable which may be expressed in terms of
#' `length(unique(df$var))` columns with levels zero or one using `model.matrix`.
#'
#' @param df A dataframe.
#' @param var A categorical variable in `df`.
#' @return A dataframe with additional columns for the dummy variables.
#' @export
mutate_dummy <- function(df, var) {
  x <- as.factor(df[[var]])
  formula <- as.formula(paste0("~ -1 + x"))
  dummy <- as.data.frame(model.matrix(formula))
  names(dummy) <- paste0(var, 1:length(unique(x)))
  cbind(df, dummy)
}

#' Check if random effect means sum-to-zero.
#'
#' @param `result` The result of a call to `multinomial_model()`.
#' @param `idx` A random effects indentifier string name such as `"area_idx"`
#' @returns A vector of sums of the random effect mean over its indicies.
#' @export
check_sum_to_zero <- function(result, idx) {
  re_mean <- result$fit$summary.random[[idx]]$mean
  colSums(matrix(re_mean, nrow = max(unique(result$df[[idx]]), na.rm = TRUE)))
}
