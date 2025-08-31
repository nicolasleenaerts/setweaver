#' cprob
#' @description Computes the conditional probability \eqn{P(Y=1 \mid X=1)} for
#' two binary vectors `y` and `x`. Rows with missing values in either vector are
#' excluded.
#'
#' @param y A binary outcome vector (0/1 or logical). Must be the same length as
#'   `x`.
#' @param x A binary predictor vector (0/1 or logical). Must be the same length
#'   as `y`.
#'
#' @return A numeric scalar giving the conditional probability that `y = 1`
#' given `x = 1`.
#' @export
#'
#' @examples
#' cprob(misimdata$y,misimdata$x1)
cprob <- function(y, x) {
  CP <- ((base::length(base::which(x == 1 & y == 1)))/(base::length(stats::na.omit(x))))/(base::length(base::which(x == 1))/base::length(stats::na.omit(x)))
  return(CP)
}
