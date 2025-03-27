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
  PX <- (length(which(x == 1)))/(length(na.omit(x)))
  SE <- sqrt((PX * (1 - PX))/length(na.omit(x)))
  Z <- PX/SE
  return(Z)
}
