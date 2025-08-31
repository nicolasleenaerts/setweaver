#' joint
#' @description
#' Computes the joint probability \eqn{P(X = 1, Y = 1)} for two binary vectors
#' `x` and `y`. Rows with missing values in either vector are excluded.
#' @param y A binary outcome vector (0/1 or logical). Must be the same length as `x`.
#' @param x A binary predictor vector (0/1 or logical). Must be the same length as `y`.
#'
#' @return
#' A numeric scalar giving the joint probability that both `x = 1` and `y = 1`,
#' calculated as the joint count divided by the number of complete cases.
#' @export
#'
#' @examples
#' joint(misimdata$y,misimdata$x1)
joint <- function(y, x) {
  JP <- (base::length(base::which(x[stats::complete.cases(x, y) == TRUE] == 1 & y[stats::complete.cases(x, y) == TRUE] == 1)))/(base::length(x[stats::complete.cases(x, y) == TRUE]))
  return(JP)
}
