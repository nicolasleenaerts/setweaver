#' zprob
#' @description Computes the z-score for testing whether the proportion (probability) of
#'   successes in `x` differs from zero.
#'
#' @param x A numeric or logical vector representing binary outcomes
#'   (e.g., 0/1 or TRUE/FALSE), from which the proportion is calculated.
#'   
#' @return A numeric value giving the z-score for the observed proportion.
#' @export
#'
#' @examples
#' zprob(misimdata$x1)
zprob <- function(x) {
  PX <- (base::length(base::which(x == 1)))/(base::length(stats::na.omit(x)))
  SE <- base::sqrt((PX * (1 - PX))/base::length(stats::na.omit(x)))
  Z <- PX/SE
  return(Z)
}
