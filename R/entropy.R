#' entropy
#' @description Returns marginal entropy for binary variables
#' @param x Variable (vector)
#'
#' @return A numerical value
#' @export
#'
#' @examples
#' entropy(misimdata$x1)
entropy <- function(x) {
  P <- length(which(x > 0))/length(na.omit(x))
  E <- -((P*log2(P)) + ((1-P)*log2(1-P)))
  return(E)
}
