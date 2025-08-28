#' setmapmi
#' @description A function that creates a setmap based on the output of the pairmi function
#'
#' @param original.variables The names of the original variables that were paired up (Vector of strings)
#' @param sets The information on the sets from the pairmi function (Data.frame object)
#' @param n_elements The depth of sets that you want to visualize (Integer)
#'
#' @return A setmap showing which original variables make up the sets at a certain depth
#' @export
#'
#' @examples
#' pairmiresult = pairmi(misimdata[,2:6])
#' setmapmi(pairmiresult$original.variables,pairmiresult$sets,2)
setmapmi <- function(original.variables = NULL,sets=NULL,n_elements=NULL){

  # Extract pairs from the requested pair level
  sets = base::subset(sets,sets$n_elements==n_elements)

  # Split pairs into sets
  combos = base::sapply(original.variables,function(x){base::apply(sets['set'],1,function(y){base::grepl(x,y)})})
  combos = base::apply(combos,1, function(x) base::list(base::names(base::which(x))))
  combos = base::unname(base::lapply(combos, "[[", 1))

  # Create Venn object
  sets = RVenn::Venn(combos)

  # Create setmap
  return(RVenn::setmap(sets))
}

