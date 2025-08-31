#' entfuns
#' @description Computes a set of descriptive diagnostics for a binary outcome
#'   `y` against one or more predictors in `x`, including marginal probability,
#'   conditional probability, absolute and proportional differences between
#'   marginal and conditional probabilities, and analogous measures based on
#'.  entropy.
#'
#' @details Inputs are treated as binary (0/1 or logical). Missing values are
#' removed pairwise for each predictor (rows with `NA` in either the outcome or
#' the predictor are excluded for that predictor's calculations).
#'
#' @param y A binary outcome vector (0/1 or logical). Length `n`.
#' @param x A data frame of binary predictors (columns). Must have `n` rows;
#'   each column is analyzed separately against `y`.
#'
#' @return A data frame with one row per predictor and the following columns:
#' \describe{
#'   \item{xvar}{Predictor name.}
#'   \item{yprob}{Marginal probability \eqn{P(Y=1)} computed on complete cases
#'     for that predictor.}
#'   \item{xprob}{Marginal probability \eqn{P(X=1)}.}
#'   \item{cprob}{Conditional probability \eqn{P(Y=1 \mid X=1)}.}
#'   \item{cpdif}{Absolute difference \eqn{P(Y=1 \mid X=1) - P(Y=1)}.}
#'   \item{cpdifper}{Percent difference relative to \eqn{P(Y=1)}.}
#'   \item{yent}{Entropy \eqn{H(Y)}.}
#'   \item{ce}{Conditional entropy \eqn{H(Y \mid X)}.}
#'   \item{cedif}{Absolute difference \eqn{H(Y) - H(Y \mid X)}.}
#'   \item{cedifper}{Percent difference in entropy relative to \eqn{H(Y)}.}
#' }
#'
#' @export
#'
#' @examples
#' entfuns(misimdata$y,misimdata[,2:5])
entfuns <- function(y,x){
  xvars= base::data.frame(xvars=base::colnames(x))
  dat = base::apply(x,2,function(x) {
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
    base::cbind(yprob,xprob,cprob,cprobx,cprobi,cpdif,cpdifper,xent,yent,ce,cedif,cedifper)
  },simplify=F)
  dat = base::cbind(xvars,base::do.call(base::rbind, dat))
  return(dat)
}
