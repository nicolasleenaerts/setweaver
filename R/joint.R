#' joint
#' @description Joint probability, P(Y∩X)
#' @param y Outcome (vector)
#' @param x Predictor (vector)
#'
#' @return A numerical value
#' @export
#'
#' @examples
#' joint(misimdata$y,misimdata$x1)
joint <- function(y, x) {
  JP <- (base::length(base::which(x[stats::complete.cases(x, y) == TRUE] == 1 & y[stats::complete.cases(x, y) == TRUE] == 1)))/(base::length(x[stats::complete.cases(x, y) == TRUE]))
  return(JP)
}
