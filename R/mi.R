#' mi
#' @description Computes the mutual information (MI) between an outcome `y` and
#' a predictor `x`, using the standard definition: \deqn{MI(X, Y) = H(X) + H(Y)
#' - H(X, Y),}
#'
#' @param y Outcome vector (binary: 0/1 or logical).
#' @param x Predictor vector (binary: 0/1 or logical). Must be the same length
#'   as `y`.
#'
#' @return A numeric scalar giving the mutual information between `x` and `y`
#'
#' @export
#'
#' @examples
#' mi(misimdata$y,misimdata$x1)
mi <- function(y, x) {
  PX <- base::length(base::which(x == 1))/base::length(stats::na.omit(x))
  PY <- base::length(base::which(y == 1))/base::length(stats::na.omit(y))
  HX <- -((PX * base::log2(PX)) + ((1 - PX) * base::log2(1-PX)))
  HY <- -((PY * base::log2(PY)) + ((1 - PY) * base::log2(1-PY)))
  JY1X1 <- ((base::length(base::which(y == 1 & x == 1)))/(base::length(stats::na.omit(x))))
  JY0X1 <- ((base::length(base::which(y == 0 & x == 1)))/(base::length(stats::na.omit(x))))
  JY1X0 <- ((base::length(base::which(y == 1 & x == 0)))/(base::length(stats::na.omit(x))))
  JY0X0 <- ((base::length(base::which(y == 0 & x == 0)))/(base::length(stats::na.omit(x))))
  A <- base::ifelse(JY1X1 == 0, 0, (JY1X1 * base::log2(JY1X1)))
  B <- base::ifelse(JY0X1 == 0, 0, (JY0X1 * base::log2(JY0X1)))
  C <- base::ifelse(JY1X0 == 0, 0, (JY1X0 * base::log2(JY1X0)))
  D <- base::ifelse(JY0X0 == 0, 0, (JY0X0 * base::log2(JY0X0)))
  HXY <- -(A + B + C + D)
  MI <- base::ifelse(PX == 0, NA, ((HX + HY) - HXY))
  return(MI)
}
