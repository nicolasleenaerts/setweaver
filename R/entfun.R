#' entfun
#' @description Run N conditional entropy estimates against a single dependent variable
#' @param y Outcome (vector)
#' @param x Predictor (data.frame object)
#'
#' @return A data.frame object
#' @export
#'
#' @examples
#' entfun(misimdata$y,misimdata[,2:5])
entfun <- function(y, x){
  dat = data.frame(xvars=colnames(x), ce = numeric(ncol(x)), stringsAsFactors = FALSE)
  for (i in 1:nrow(dat)){
    dat$ce[i] = ce(y, x[[i]])
  }
  return(dat)
}
