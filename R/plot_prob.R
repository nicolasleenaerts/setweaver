#' plot_prob
#'
#' @description Creates a network-style graph showing how a set of predictors
#' (`x_vars`) are related to an outcome (`y_var`). Relationships can be
#' displayed either as conditional probabilities or as effects estimated by
#' logistic regression.
#'
#' @param data A data frame containing the outcome (`y_var`) and predictors
#'   (`x_vars`).
#' @param y_var Character string giving the name of the outcome variable in
#'   `data`.
#' @param x_vars Character vector of predictor variable names in `data`.
#' @param var_labels Optional character vector of display labels for the
#'   predictors. Must match the length of `x_vars`.
#' @param prob_digits Integer; number of decimal places to round conditional
#'   probabilities. Defaults to `2`.
#' @param method Character string indicating how to quantify associations:
#'   `"prob"` for conditional probabilities or `"logistic"` for logistic
#'   regression effects.
#' @param title Character string; title of the plot.
#' @param min_arrow_width Numeric value for the minimum edge width.
#' @param max_arrow_width Numeric value for the maximum edge width.
#' @param node_size Numeric value controlling the size of nodes.
#' @param label_cex Numeric value controlling the size of node labels.
#' @param vertex_color Character string giving the fill color of nodes.
#' @param vertex_frame_color Character string giving the color of node borders.
#' @param vertex_label_color Character string giving the color of node labels.
#' @param edge_color Character string giving the color of edges.
#' @param edge_label_color Character string giving the color of edge labels.
#'
#' @return A graph object (typically an [`igraph::igraph`] object or similar) is
#' returned and plotted. Nodes represent variables and edges represent
#' associations. Node labels include variable names and marginal probabilities.
#' Edge labels display either conditional probabilities or logistic regression
#' effects.
#'
#' @export
#' @examples
#' plot_prob(misimdata,'y',colnames(misimdata[,3:6]),method='logistic')

plot_prob <- function(data,
                       y_var,
                       x_vars,
                       var_labels = NULL,
                       prob_digits = 2, 
                       method='conditional',
                       title = NULL,
                       vertex_color = "lightblue",
                       vertex_frame_color = "darkblue",
                       vertex_label_color = "black",
                       edge_color = "darkgrey",
                       edge_label_color = "black",
                       min_arrow_width = 1,
                       max_arrow_width = 10,
                       node_size = 40,
                       label_cex = 0.8) {
  
  # --- Input Validation ---
  
  # Check if data is a dataframe
  if (!base::inherits(data, "data.frame")) base::stop("Input 'data' must be a data frame.")
  
  # Check if the outcome is a single string
  if (!base::is.character(y_var) || base::length(y_var) != 1) base::stop("'y_var' must be a single string.")
  
  # Allow x_vars to be empty for case with only Y
  if (!base::is.character(x_vars) ) base::stop("'x_vars' must be a character vector.")
  
  # Check if outcome is in the data
  if (!y_var %in% base::names(data)) base::stop("'", y_var, "' not found in data frame.")
  
  # Check if predictors are in the data
  if (base::length(x_vars) > 0 && !base::all(x_vars %in% base::names(data))) {
    missing_vars <- x_vars[!x_vars %in% base::names(data)]
    base::stop("Following 'x_vars' not found in data frame: ", base::paste(missing_vars, collapse=", "))
  }
  
  #  Check if the labels for the variables are a names vector or list or NULL
  if (!base::is.null(var_labels) && !base::is.list(var_labels) && !base::is.vector(var_labels)) {
    base::stop("'var_labels' must be a named vector or list, or NULL.")
  }
  
  #Check if the labels for the variables are a names vector or list
  if (!base::is.null(var_labels) && base::is.null(names(var_labels))) {
    base::stop("'var_labels' must be a *named* vector or list.")
  }
  
  # Check if variables are binary (or logical)
  all_vars_to_check <- base::c(y_var, x_vars)
  
  # Check whether the outcome is binary Y var
  if (!check_binary(data,y_var)) base::stop("Y variable '", y_var, "' must be binary (0/1 or TRUE/FALSE). Found values: ", base::paste(utils::head(base::unique(data[[y_var]])), collapse=", "))
  
  # Check whether X vars are binary only if they exist
  if (base::length(x_vars) > 0) {
    non_binary_x <- x_vars[!base::sapply(x_vars, function(x){check_binary(data,x)})]
    if (base::length(non_binary_x) > 0) {
      non_binary_vals <- base::sapply(non_binary_x, function(v) base::paste(utils::head(base::unique(data[[v]])), collapse=","))
      stop("X variables must be binary (0/1 or TRUE/FALSE). Non-binary found: ",
           base::paste(base::paste0(non_binary_x, " (values: ", non_binary_vals, ")"), collapse="; "))
    }
  }

  
  # --- Calculate Conditional Probabilities ---
  
  if (method=='conditional'){
    
    # Ensure data is numeric 0/1 for calculations
    selected_data = data |>
      dplyr::select(dplyr::all_of(all_vars_to_check)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), base::as.numeric))
    
    # Conditional Probabilities P(Y=1 | Xi=1)
    cond_probs <- base::list()
    if(base::length(x_vars) > 0) {
      for (x_var in x_vars) {
        # Subset data where Xi=1
        subset_data <- dplyr::filter(selected_data[[x_var]] == 1)
        
        if (base::nrow(subset_data) > 0) {
          # Calculate P(Y=1) within the subset
          cond_probs[[x_var]] <- base::mean(subset_data[[y_var]], na.rm = TRUE)
        } else {
          # Handle case where Xi never occurs (P(Xi)=0)
          cond_probs[[x_var]] <- NA # Keep as NA for now
          warning("Variable '", x_var, "' has no occurrences (P(", x_var, ")=0). Conditional probability P(Y|", x_var, ") set to NA.")
        }
      }
    }
    probs <- base::unlist(cond_probs)
  }
  
  # --- Perform Logistic Regression ---
  if (method=='logistic'){
    
    # Select data and ensure numeric 0/1
    selected_data <- data |>
      dplyr::select(dplyr::all_of(all_vars_to_check)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), base::as.numeric)) |>
      stats::na.omit() # IMPORTANT: Remove rows with NAs for GLM
    
    if(base::nrow(selected_data) < 2) {
      base::stop("Insufficient non-NA data (need at least 2 rows) to fit logistic regression.")
    }
    # Check for variance after removing NA
    if (base::length(base::unique(selected_data[[y_var]])) < 2) {
      base::stop("Outcome variable '", y_var,"' has no variance after removing NAs. Cannot fit model.")
    }
    no_variance_x <- x_vars[base::sapply(x_vars, function(v) base::length(base::unique(selected_data[[v]])) < 2)]
    if (base::length(no_variance_x) > 0) {
      base::warning("Predictor variable(s) have no variance after removing NAs: ", paste(no_variance_x, collapse=", "), ". They will be excluded from the model.")
      x_vars <- base::setdiff(x_vars, no_variance_x) # Remove them
      if (base::length(x_vars) == 0) {
        base:: stop("No predictor variables with variance remaining after removing NAs.")
      }
    }
    
    formula_str <- base::paste(y_var, "~", base::paste(x_vars, collapse = " + "))
    logit_model <- tryCatch({
      stats::glm(stats::as.formula(formula_str), data = selected_data, family = stats::binomial(link = "logit"))
    }, error = function(e) {
      base::stop("Error fitting logistic regression model: ", e$message)
    })
    
    model_summary <- base::summary(logit_model)
    raw_coeffs <- stats::coefficients(model_summary) # Matrix with Estimate, Std. Error etc.
    
    # --- Extract Coefficients and Calculate Effect Probabilities ---
    logit_coeffs_est <- base::numeric(length(x_vars))
    base::names(logit_coeffs_est) <- x_vars
    missing_coeffs_vars <- base::character()
    
    for(x_var in x_vars) {
      if (x_var %in% base::rownames(raw_coeffs)) {
        # Handle potential NA coefficients (e.g., perfect separation/collinearity)
        if (base::is.na(raw_coeffs[x_var, "Estimate"])) {
          warning("Coefficient for '", x_var, "' is NA (possibly due to collinearity or separation). Setting effect probability to 0.5.")
          logit_coeffs_est[x_var] <- 0 # Treat as no effect if NA
        } else {
          logit_coeffs_est[x_var] <- raw_coeffs[x_var, "Estimate"]
        }
      } else {
        # Should not happen if variable had variance, but check anyway
        warning("Coefficient for '", x_var, "' not found in model summary (unexpected). Setting effect probability to 0.5.")
        logit_coeffs_est[x_var] <- 0
        missing_coeffs_vars <- base::c(missing_coeffs_vars, x_var)
      }
    }
    
    # Inverse logit function
    inv_logit <- function(x) { base::exp(x) / (1 + base::exp(x)) }
    
    # Calculate effect probabilities from logit coefficients
    probs <- inv_logit(logit_coeffs_est)
  }
  
  
  # --- Calculate Marginal Probabilities ---
  
  # Using original data before NA removal for consistency
  marg_data_selected <- data |>
    dplyr::select(dplyr::all_of(c(y_var, x_vars))) |> # Select original x_vars (incl. those removed)
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  
  # Marginal Probabilities
  marginal_probs = selected_data |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ base::mean(.x, na.rm = TRUE)))
  p_y = marginal_probs[[y_var]]
  
  # Handle case where there are no x_vars
  p_x = if(base::length(x_vars) > 0) {
    marginal_probs |> dplyr::select(dplyr::all_of(x_vars))
  } else {
    base::numeric(0) # Empty numeric vector
  }
  
  # --- Prepare for igraph ---
  
  # Node Information
  node_names <- base::c(y_var, x_vars)
  
  # Ensure marginal probabilities align with node_names (handle removed vars)
  marg_p_values_ordered <- base::numeric(base::length(node_names))
  base::names(marg_p_values_ordered) <- node_names
  marg_p_values_ordered[y_var] <- p_y
  for(xn in x_vars){
    if(xn %in% base::names(p_x)){
      marg_p_values_ordered[xn] <- p_x[[xn]]
    } else {
      marg_p_values_ordered[xn] <- NA # Should not happen if selected correctly
    }
  }
  # Handle potential NAs if a variable was all NA originally
  marg_p_values_ordered[base::is.na(marg_p_values_ordered)] <- 0
  marg_p_values <- marg_p_values_ordered
  
  # Use sapply to apply the function to all node_names
  display_names <- base::sapply(node_names, function(x){get_display_name(var_labels,x)}, USE.NAMES = FALSE)
  
  # Create node labels using display names and formatted probabilities
  node_labels <- base::sprintf("P(%s)\n%.*f", display_names, prob_digits, marg_p_values)
  
  nodes <- base::data.frame(
    name = node_names, # Internal name for igraph structure
    label = node_labels, # Visual label for the plot
    stringsAsFactors = FALSE
  )
  # Edge Information (only for x_vars included in the final model)
  valid_x_vars <- names(probs) # These are the vars with calculated effect probs
  
  # Edge Information
  if (length(x_vars) > 0) {
    edges <- base::data.frame(
      from = x_vars,
      to = y_var,
      probs = probs,
      # Format edge labels using prob_digits
      label = base::sprintf("%.*f", prob_digits, probs),
      stringsAsFactors = FALSE
    )
    
    # Handle cases where P(Xi)=0 resulted in NA probs for label
    edges$label[base::is.na(edges$probs)] <- "NA"
    
    # Scale probability to arrow width
    if (method == 'conditional'){
      strength <- edges$probs[!base::is.na(edges$probs)]
      lower_bound <- if(base::length(strength) > 0) base::min(strength) else 0
      upper_bound <- if(base::length(strength) > 0) base::max(strength) else 0
    }
    if (method == 'logistic'){
      strength <- base::abs(edges$probs - 0.5)
      lower_bound <- base::min(strength, na.rm = TRUE)
      upper_bound <- base::max(strength, na.rm = TRUE)
    }
    
    if (length(strength) == 0 || upper_bound == lower_bound) {
      # Assign average width if all effects are the same strength or only one edge
      edges$width <- (min_arrow_width + max_arrow_width) / 2
    } else if (upper_bound > lower_bound) {
      # Scale effect strength linearly to width range
      edges$width <- min_arrow_width + (strength - lower_bound) / (upper_bound - lower_bound) * (max_arrow_width - min_arrow_width)
    } else {
      # Fallback (should not happen with valid strength values)
      edges$width <- min_arrow_width
    }
    # Ensure NAs (shouldn't exist here) get minimum width
    edges$width[base::is.na(edges$width)] <- min_arrow_width
    
  } else {
    # No valid X variables remained for the model
    edges <- base::data.frame(from=base::character(), to=base::character(), effect_prob=base::numeric(), label=base::character(), width=base::numeric(), stringsAsFactors = FALSE)
    warning("No predictors remained in the model to draw edges.")
  }
  
  # --- Create and Plot Graph ---
  # Nodes dataframe might contain nodes for x_vars that were removed from model
  # graph_from_data_frame handles this; only nodes connected by edges or listed in 'vertices' are added.
  # We pass the full 'nodes' df to ensure all original vars appear, even if disconnected.
  g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  
  # Set layout
  l <- NULL # Initialize layout variable
  if (igraph::vcount(g) > 0) {
    if (y_var %in% igraph::V(g)$name && base::length(x_vars) > 0) { # Ensure Y is center only if there are X's
      center_node_index <- which(igraph::V(g)$name == y_var)
      if (length(center_node_index) > 0) {
        l <- igraph::layout_as_star(g, center = center_node_index)
      } else {
        warning("Y variable '", y_var, "' specified but not found as a vertex. Using default layout.")
        l <- igraph::layout_nicely(g)
      }
    } else { # If only Y node, or Y not found, or graph disconnected
      l <- igraph::layout_nicely(g) # Nicely usually works well for single nodes or disconnected graphs
    }
  } else {
    # Handle empty graph case (no nodes/edges) - create dummy layout
    l <- base::matrix(base::numeric(0), ncol=2)
    warning("Graph is empty (no nodes or edges). Nothing to plot.")
  }
  
  # Set title
  if (base::is.null(title)==T){
    if (method=='conditional'){title="Conditional Probability Network"}
    if (method=='logistic'){title="Logistic Regression Effect Probability Network"}
  }
  
  # Only plot if the graph is not empty
  if (igraph::vcount(g) > 0) {
    # Ensure layout is valid before plotting
    if (base::is.null(l) || !base::is.matrix(l) || base::nrow(l) != igraph::vcount(g)) {
      warning("Layout calculation failed. Attempting default layout.")
      l <- igraph::layout_nicely(g) # Fallback layout
    }
    
    plot(g,
         layout = l,
         main = title,
         vertex.shape = "circle",
         vertex.size = node_size,
         vertex.color = vertex_color,
         vertex.frame.color = NA,
         vertex.label = igraph::V(g)$label,
         vertex.label.color = vertex_label_color,
         vertex.label.cex = label_cex,
         edge.color = edge_color,
         # Only set edge attributes if edges exist
         edge.width = if(igraph::ecount(g) > 0) igraph::E(g)$width else NULL,
         edge.label = if(igraph::ecount(g) > 0) igraph::E(g)$label else NULL,
         edge.label.color = edge_label_color,
         edge.label.cex = label_cex,
         edge.arrow.size = 0,
         edge.curved = 0.1
    )
  } else {
    # Create a blank plot with title if graph is empty
    plot(NULL, xlim=c(-1,1), ylim=c(-1,1), main=paste(title, "(Empty Graph)"), xlab="", ylab="", axes=FALSE)
    graphics::text(0, 0, "No data to display", cex=1.2)
  }
  
  # Return the graph object invisibly
  base::invisible(g)
}