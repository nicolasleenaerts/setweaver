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
  P <- base::length(base::which(x > 0))/base::length(stats::na.omit(x))
  E <- -((P*base::log2(P)) + ((1-P)*base::log2(1-P)))
  return(E)
}
