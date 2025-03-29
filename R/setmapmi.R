#' setmapmi
#' @description A function that creates a setmap based on the output of the pairmi function
#'
#' @param original.variables The names of the original variables that were paired up (Vector of strings)
#' @param sets The information on the sets from the pairmi function (Data.frame object)
#' @param n_elements The depth of sets that you want to visualize (Integer)
#'
#' @return A setmap showing which original variables make up the sets at a certain depth
#' @import dplyr
#' @export
#'
#' @examples
#' setmapmi(pairmiresults$original.variables,pairmiresults$sets,2)
setmapmi <- function(original.variables = NULL,sets=NULL,n_elements=NULL){

  # Load dplyr
  require(dplyr)

  # Extract pairs from the requested pair level
  sets = subset(sets,sets$n_elements==n_elements)

  # Split pairs into sets
  combos = data.frame(sets['set'] %>% rowwise() %>% mapply(grepl,original.variables,.))
  combos = apply(combos,1, function(x) list(names(which(x))))
  combos = lapply(combos, "[[", 1)

  # Create Venn object
  sets = RVenn::Venn(combos)

  # Create setmap
  return(RVenn::setmap(sets))
}

