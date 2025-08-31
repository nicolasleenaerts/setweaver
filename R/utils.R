#' utils.R
# Internal utility functions for the setweaver package
#'
#' @keywords internal
#' @noRd

# Check if a variable is binary
check_binary <- function(data,col_name) {
  vals <- tryCatch(base::unique(stats::na.omit(data[[col_name]])), error = function(e) NULL)
  if (base::is.null(vals)) return(FALSE) # Column likely didn't exist or had only NAs
  # Allow empty vectors (e.g. if X never occurs and has only NAs)
  if (base::length(vals) == 0) return(TRUE)
  base::is.logical(vals) || (base::is.numeric(vals) && base::all(vals %in% c(0, 1)))
}

# Generate display names based on var_labels if provided
get_display_name <- function(var_labels, name) {
  
  
  # Check if var_labels is provided and the name exists within it
  if (!base::is.null(var_labels) && !base::is.null(names(var_labels)) && name %in% base::names(var_labels)) {
    # Return the corresponding label value
    return(var_labels[[name]])
  } else {
    # If var_labels *was* provided but the name wasn't found, issue a warning
    if (!base::is.null(var_labels) && !base::is.null(names(var_labels)) && !(name %in% base::names(var_labels))) {
      warning("Label not found for variable '", name, "' in 'var_labels'. Using original name.")
    }
    # Default to original name if var_labels is NULL or name not found
    return(name)
  }
}