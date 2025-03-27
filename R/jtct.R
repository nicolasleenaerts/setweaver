#' jtct
#' @description Joint count function
#' @param y Outcome (vector)
#' @param x Predictor (vector)
#'
#' @return An integer value
#' @export
#'
#' @examples
#' cprob_inv(misimdata$y,misimdata$x1)
jtct <- function(y, x) {
  JTCT <- length(which(x[complete.cases(x, y) == TRUE] == 1 & y[complete.cases(x, y) == TRUE] == 1))
  return(JTCT)
}
