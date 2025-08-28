#' zprob
#' @description z score for proportion (probability) differing from zero. To insure that distress is occurring a nonzero proportion of time
#'
#' @param x Variable (vector)
#'
#' @return A numerical value
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
