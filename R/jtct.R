#' jtct
#' @description Counts the number of complete observations where both a binary
#' outcome `y` and a binary predictor `x` equal 1. Missing values are excluded
#' pairwise (rows with `NA` in either `x` or `y` are ignored).
#'
#' @param y Outcome vector (binary: 0/1 or logical). Must be the same length as
#'   `x`.
#' @param x Predictor vector (binary: 0/1 or logical). Must be the same length
#'   as `y`.
#'
#' @return An integer scalar giving the number of observations where `x == 1`
#' and `y == 1`, after excluding missing values.
#' @export
#'
#' @examples
#' cprob_inv(misimdata$y,misimdata$x1)
jtct <- function(y, x) {
  JTCT <- base::length(base::which(x[stats::complete.cases(x, y) == TRUE] == 1 & y[stats::complete.cases(x, y) == TRUE] == 1))
  return(JTCT)
}
