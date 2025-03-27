#' mi2
#' @description Additional formulation, where: I(X, Y) = H(Y) − H(Y|X), P's = probability, H's = entropy, J's = joint probabilities
#' @param y Outcome (vector)
#' @param x Predictor (vector)
#'
#' @return A numerical value
#' @export
#'
#' @examples
#' mi2(misimdata$y,misimdata$x1)
mi2 <- function(y, x) {
  PX <- length(which(x == 1))/length(na.omit(x))
  PY <- length(which(y == 1))/length(na.omit(y))
  HX <- -((PX * log2(PX)) + ((1 - PX) * log2(1-PX)))
  HY <- -((PY * log2(PY)) + ((1 - PY) * log2(1-PY)))
  JY1X1 <- ((length(which(y == 1 & x == 1)))/(length(na.omit(x))))
  JY0X1 <- ((length(which(y == 0 & x == 1)))/(length(na.omit(x))))
  JY1X0 <- ((length(which(y == 1 & x == 0)))/(length(na.omit(x))))
  JY0X0 <- ((length(which(y == 0 & x == 0)))/(length(na.omit(x))))
  A <- ifelse(JY1X1 == 0, 0, (JY1X1 * log2(JY1X1)))
  B <- ifelse(JY0X1 == 0, 0, (JY0X1 * log2(JY0X1)))
  C <- ifelse(JY1X0 == 0, 0, (JY1X0 * log2(JY1X0)))
  D <- ifelse(JY0X0 == 0, 0, (JY0X0 * log2(JY0X0)))
  HXY <- -(A + B + C + D)
  CE <- ifelse(PX == 0, NA, HXY - HX)
  MI <- ifelse(PX == 0, NA, HY - CE)
  return(MI)
}
