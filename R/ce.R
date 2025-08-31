#' ce
#' @description
#' Computes the conditional entropy \eqn{H(Y \mid X)} for two binary
#' vectors `y` (outcome) and `x` (predictor).
#'
#' @param y A binary outcome vector (0/1 or logical). Must be the same length as `x`.
#' @param x A binary predictor vector (0/1 or logical). Must be the same length as `y`.
#'
#' @return
#' A numeric scalar giving \eqn{H(Y \mid X)}.
#'
#' @export
#'
#' @examples
#' ce(misimdata$y,misimdata$x1)
ce <- function(y, x) {
  PX <- base::length(base::which(x == 1))/base::length(stats::na.omit(x))
  HX <- -((PX * base::log2(PX)) + ((1 - PX) * base::log2(1-PX)))
  JY1X1 <- ((base::length(base::which(y == 1 & x == 1)))/(base::length(stats::na.omit(x))))
  JY0X1 <- ((base::length(base::which(y == 0 & x == 1)))/(base::length(stats::na.omit(x))))
  JY1X0 <- ((base::length(base::which(y == 1 & x == 0)))/(base::length(stats::na.omit(x))))
  JY0X0 <- ((base::length(base::which(y == 0 & x == 0)))/(base::length(stats::na.omit(x))))
  A <- base::ifelse(JY1X1 == 0, 0, (JY1X1 * base::log2(JY1X1)))
  B <- base::ifelse(JY0X1 == 0, 0, (JY0X1 * base::log2(JY0X1)))
  C <- base::ifelse(JY1X0 == 0, 0, (JY1X0 * base::log2(JY1X0)))
  D <- base::ifelse(JY0X0 == 0, 0, (JY0X0 * base::log2(JY0X0)))
  HXY <- -(A + B + C + D)
  CE <- base::ifelse(PX == 0, NA, HXY - HX)
  return(CE)
}
