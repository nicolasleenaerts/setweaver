#' setmapmi
#' @description A function that creates a setmap based on the output of the pairmi function
#'
#' @param orig.variables The names of the original variables that were paired up (Vector of strings)
#' @param pairs The information on the pairs from the pairmi function (Data.frame object)
#' @param depth The depth of sets that you want to visualize (Integer)
#'
#' @return A setmap showing which original variables make up the sets at a certain depth
#' @import dplyr
#' @export
#'
#' @examples
#' setmapmi(pairmiresults$orig.variables,pairmiresults$pairs,2)
setmapmi <- function(orig.variables = NULL,pairs=NULL,depth=NULL){

  # Load dplyr
  require(dplyr)

  # Extract pairs from the requested pair level
  depth.requested = depth
  pairs = subset(pairs,pairs$depth==depth.requested)

  # Split pairs into sets
  combos = data.frame(pairs['pair'] %>% rowwise() %>% mapply(grepl,orig.variables,.))
  combos = apply(combos,1, function(x) list(names(which(x))))
  combos = lapply(combos, "[[", 1)

  # Create Venn object
  sets = RVenn::Venn(combos)

  # Create setmap
  return(RVenn::setmap(sets))
}
