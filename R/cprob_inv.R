#' cprob_inv
#' @description Conditional Probability of the inverse, where: P(Y|X) = P(Y∩X)/P(X=0)
#' @param y Outcome (vector)
#' @param x Predictor (vector)
#'
#' @return A numerical value
#' @export
#'
#' @examples
#' cprob_inv(misimdata$y,misimdata$x1)
cprob_inv <- function(y, x) {
  CP <- ((length(which(x == 0 & y == 1)))/(length(na.omit(x))))/(length(which(x == 0))/length(na.omit(x)))
  return(CP)
}
