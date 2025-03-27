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
  JP <- (length(which(x[complete.cases(x, y) == TRUE] == 1 & y[complete.cases(x, y) == TRUE] == 1)))/(length(x[complete.cases(x, y) == TRUE]))
  return(JP)
}
