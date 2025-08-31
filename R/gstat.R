#' gstat
#' @description 
#' Computes the likelihood-ratio test statistic (G statistic) from the mutual
#' information and the joint count of two variables:
#' \deqn{G = 2 \times n \times MI,}
#' where \eqn{n} is the joint sample size and \eqn{MI} is the mutual information.
#'
#' @param mi Numeric scalar; the mutual information between two variables.
#' @param count Integer scalar; the joint count (sample size) used in computing
#'   \code{mi}.
#' @return 
#' A numeric scalar giving the G statistic value.
#' @export
#'
#' @examples
#' gstat(mi(misimdata$y,misimdata$x1),jtct(misimdata$y,misimdata$x1))
gstat <- function(mi, count) {
  G <- 2*count*mi
  return(G)
}
