#' find_minimal_sets
#' 
#' @description
#' Given a character vector of sets (each set encoded as variable names joined by
#' a separator), returns the subset of sets that are minimal: no returned set
#' is a strict superset of another. Duplicates and ordering differences are
#' handled according to the implementation.
#' 
#' @param str_vec Character vector of set strings for which to find minimally
#'   sufficient sets (e.g., `c("x1_x2", "x1_x2_x3")`).
#' @param sep Character string used as the separator between variables in each
#'   set. Defaults to `"_"`.
#'
#' @return
#' A character vector containing the minimally sufficient sets (i.e., sets that
#' are not strict supersets of any other set in `str_vec`).
#' 
#' @export
#' @examples
#' pairmiresult = pairmi(misimdata[,2:6])
#' results_probstat <- probstat(misimdata$y,pairmiresult$expanded.data,nfolds=5)
#' find_minimal_sets(results_probstat$xvars[results_probstat$cprob >= 0.20])
find_minimal_sets <- function(str_vec,sep='_') {
  # Remove sets for which parts can be explained by an entire other set
  str_vec[!
            # For every set
            base::sapply(c(1:base::length(str_vec)),function(i){
              # Again for every set
              for (j in 1:base::length(str_vec)) {
                if (i != j) {
                  minimal <- base::all(base::strsplit(str_vec[j],split= sep)[[1]] %in% base::strsplit(str_vec[i],split= sep)[[1]])
                  if (minimal==T) {break}
                }
              }
              return(minimal)
            })]
}






