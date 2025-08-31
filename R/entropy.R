#' entropy
#' @description Returns marginal entropy for binary variables
#' @param x A binary vector (numeric coded as 0/1 or logical). Must be length ≥ 1.
#'
#' @return
#' A numeric scalar giving the entropy of `x`.
#' @export
#'
#' @examples
#' entropy(misimdata$x1)
entropy <- function(x) {
  P <- base::length(base::which(x > 0))/base::length(stats::na.omit(x))
  E <- -((P*base::log2(P)) + ((1-P)*base::log2(1-P)))
  return(E)
}
