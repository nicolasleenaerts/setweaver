#' setmapmi
#'
#' @description Creates a set map visualization from the output of [pairmi()],
#' showing which original variables compose the derived sets at a specified
#' depth.
#'
#' @param original_variables Character vector of names for the original
#'   variables that were paired (typically `pairmi_result$original.variables`).
#' @param sets A data frame returned by [pairmi()] describing the sets. Must
#'   contain the columns required by `setmapmi()` (e.g., identifiers for sets
#'   and their constituent variables).
#' @param n_elements Integer scalar giving the set size (depth) to visualize
#'   (e.g., `2` for pairs, `3` for triplets). Must be >= 1 and present in
#'   `sets`.
#'
#' @return A setmap showing which original variables make up the sets at a
#'   certain depth
#' @export
#'
#' @examples
#' pairmiresult = pairmi(misimdata[,2:6])
#' setmapmi(pairmiresult$original.variables,pairmiresult$sets,2)
setmapmi <- function(original_variables = NULL,sets=NULL,n_elements=NULL){

  # Extract pairs from the requested pair level
  sets = base::subset(sets,sets$n_elements==n_elements)

  # Split pairs into sets
  combos = base::sapply(original_variables,function(x){base::apply(sets['set'],1,function(y){base::grepl(x,y)})})
  combos = base::apply(combos,1, function(x) base::list(base::names(base::which(x))))
  combos = base::unname(base::lapply(combos, "[[", 1))

  # Create Venn object
  sets = RVenn::Venn(combos)

  # Create setmap
  return(RVenn::setmap(sets))
}

