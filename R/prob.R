# Marginal probability P(X)
prob <- function(x) {
  JP <- (length(which(x == 1)))/(length(na.omit(x)))
  return(JP)
}