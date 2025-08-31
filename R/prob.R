#' prob
#'
#' @description Computes the marginal probability \eqn{P(X = 1)} for a binary
#' vector `x`, ignoring missing values.
#'
#' @param x A numeric or logical vector coded as 0/1 (or `FALSE`/`TRUE`). Values
#'   other than 0, 1, `FALSE`, `TRUE`, or `NA` will be ignored.
#'
#' @return A numeric scalar giving the proportion of entries equal to 1 among
#'   the non-missing values of `x`.
#'
#' @export
#'
#' @examples
#' prob(c(0, 1, 1, 0, 1))
prob <- function(x) {
  JP <- (base::length(base::which(x == 1)))/(base::length(stats::na.omit(x)))
  return(JP)
}
