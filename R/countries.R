#' @export
priority_iso3 <- function() {
  c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
}

#' @export
analysis_level <- function() {
  c(
    "BWA" = 3, "CMR" = 2, "KEN" = 2,
    "LSO" = 1, "MOZ" = 3, "MWI" = 5,
    "NAM" = 2, "SWZ" = 1, "TZA" = 4,
    "UGA" = 3, "ZAF" = 2, "ZMB" = 2,
    "ZWE" = 2
  )
}

#' @export
admin1_level <- function() {
  c(
    "BWA" = 1, "CMR" = 1, "KEN" = 1,
    "LSO" = 1, "MOZ" = 1, "MWI" = 1,
    "NAM" = 2, "SWZ" = 1, "TZA" = 2,
    "UGA" = 1, "ZAF" = 1, "ZMB" = 1,
    "ZWE" = 1
  )
}
