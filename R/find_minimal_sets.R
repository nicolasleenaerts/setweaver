#' find_minimal_sets
#' 
#' @description A function that finds the irreducible and nonexchangeable sets that account for all other sufficient sets at a certain conditional probability threshold
#'
#' @param str_vec Vector of sets for which you want to find the minimally sufficient sets (vector)
#' @param sep The seperator that was used in the creation of the sets (character)
#'
#' @return A vector with the minimally sufficient sets
#' 
#' @export
#' @examples
#' pairmiresult = pairmi(misimdata[,2:6])
#' results_probstat <- probstat(misimdata$y,pairmiresult$expanded.data,nfolds=5)
#' find_minimal_sets(results_probstat$xvars[results_probstat$cprob >= 0.20])

find_minimal_sets <- function(str_vec,sep='_') {
  minimal_sets <- character(0)
  
  for (i in 1:length(str_vec)) {
    set1 <- string_to_set(str_vec[i])
    minimal <- TRUE
    
    for (j in 1:length(str_vec)) {
      if (i != j) {
        set2 <- string_to_set(str_vec[j])
        if (all(set2 %in% set1)) {
          minimal <- FALSE
          break
        }
      }
    }
    if (minimal) {
      minimal_sets <- c(minimal_sets, str_vec[i])
    }
  }
  
  return(minimal_sets)
}
