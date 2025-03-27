#' gstat
#' @description G-statistic
#'
#' @param mi Mutual information (numerical value)
#' @param count Joint count (integer value)
#'
#' @return A numerical value
#' @export
#'
#' @examples
#' gstat(mi(misimdata$y,misimdata$x1),jtct(misimdata$y,misimdata$x1))
gstat <- function(mi, count) {
  G <- 2*count*mi
  return(G)
}
