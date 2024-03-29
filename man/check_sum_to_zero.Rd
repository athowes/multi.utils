% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inla-idx.R
\name{check_sum_to_zero}
\alias{check_sum_to_zero}
\title{Check if random effect means sum-to-zero.}
\usage{
check_sum_to_zero(result, idx)
}
\arguments{
\item{`result`}{The result of a call to `multinomial_model()`.}

\item{`idx`}{A random effects indentifier string name such as `"area_idx"`}
}
\value{
A vector of sums of the random effect mean over its indicies.
}
\description{
Check if random effect means sum-to-zero.
}
