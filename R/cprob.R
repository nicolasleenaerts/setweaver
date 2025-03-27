#' cprob
#' @description Conditional Probability, where: P(Y|X) = P(Y∩X)/P(X=1)
#' @param y Outcome (vector)
#' @param x Predictor (vector)
#'
#' @return A numerical value
#' @export
#'
#' @examples
#' cprob(misimdata$y,misimdata$x1)
cprob <- function(y, x) {
  CP <- ((length(which(x == 1 & y == 1)))/(length(na.omit(x))))/(length(which(x == 1))/length(na.omit(x)))
  return(CP)
}
