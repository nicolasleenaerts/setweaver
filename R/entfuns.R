#' entfuns
#' @description A diagnostic and descriptive tool that calculates probability, conditional probability, absolute difference between marginal and conditional probability, absolute difference between marginal and conditional entropy, proportional difference between marginal and conditional probability, and proportional difference between marginal and conditional entropy
#' @param y Outcome (vector)
#' @param x Predictor (data.frame object)
#'
#' @return A data.frame object
#' @export
#'
#' @examples
#' entfuns(misimdata$y,misimdata[,2:5])
entfuns <- function(y,x){
  xvars= data.frame(xvars=colnames(x))
  dat = apply(x,2,function(x) {
    xprob = prob(x)
    yprob = prob(y)
    cprob = cprob(y, x)
    cprobx = cprob(x, y)
    cprobi = cprob_inv(y, x)
    cpdif= cprob - yprob # Δ between marginal and conditional
    cpdifper = cpdif/yprob # percent Δ between marginal and conditional
    xent = entropy(x)
    yent = entropy(y)
    ce = ce(y, x)
    cedif = yent - ce # Δ between marginal and conditional
    cedifper = cedif/yent # percent Δ between marginal and conditional
    cbind(yprob,xprob,cprob,cprobx,cprobi,cpdif,cpdifper,xent,yent,ce,cedif,cedifper)
  },simplify=F)
  dat = cbind(xvars,do.call(rbind, dat))
  return(dat)
}
