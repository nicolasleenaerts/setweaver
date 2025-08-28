# Marginal probability P(X)
prob <- function(x) {
  JP <- (base::length(base::which(x == 1)))/(base::length(stats::na.omit(x)))
  return(JP)
}
