#' pairmi
#' @description A function that calculates the mutual information for sets of
#'   variables, calculates the G statistic, determines the significance of the
#'   sets, and only keeps those that are significant.
#'
#' @param data A data frame containing the variables to be paired/combined.
#'   Columns should be binary.
#' @param alpha Numeric p-value threshold for significance (default used by the
#'   implementation if not supplied).
#' @param MI.threshold Numeric mutual information threshold. If provided, it
#'   overrides `alpha`-based filtering.
#' @param n_elements Integer giving the maximum size of sets to evaluate (e.g.,
#'   `2` for pairs, `3` for triplets). Must be ≥ 2.
#' @param sep String used to join variable names when forming set identifiers
#'   (e.g., `"_"`).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{expanded.data}{A data frame containing the original variables and the
#'     columns for significant sets (e.g., pair/triplet indicators).}
#'   \item{original.variables}{Character vector of the original variable names.}
#'   \item{sets}{A data frame describing significant sets, including their
#'     members, size, MI, G statistic, p-value, and constructed name.}
#' }
#' @export
#'
#' @examples
#' pairmi(misimdata[,2:6])
pairmi <- function(data,alpha=0.05,MI.threshold=NULL,n_elements=5,sep='_'){

  # Create df to store information about the pairs
  pairs_retained_df = base::data.frame(base::matrix(ncol=5))[-1,]
  colnames(pairs_retained_df) = base::c('n_elements','set','mi','relative.mi','p')

  # Get original variables
  orig_variables = base::colnames(data)

  # Create progress bar
  base::message('Pairing Data')
  pb = utils::txtProgressBar(min = 0, max = (n_elements-1), initial = 0)

  # Create Pairs
  for (depth_level in 2:n_elements){

    # Reduce the number of variables to pair
    if (depth_level>2) {search_data=dplyr::select(data,c(dplyr::all_of(orig_variables),pairs_retained_df$set[pairs_retained_df$n_elements==(depth_level-1)]))
    } else {search_data=data}

    # Get all possible combinations of the variables
    search_df = base::data.frame(base::t(utils::combn(base::colnames(search_data), 2, simplify=TRUE)))

    # Retain only correct combinations if depth level >2
    if (depth_level >2){

      # Only retain pairs with a variable of the original data and a variable of the previous depth level
      search_df$orig_variable_in_pair = base::ifelse(search_df$X1%in%orig_variables|search_df$X2%in%orig_variables,1,0)
      search_df$highest_depth_in_pair = base::ifelse(search_df$X1%in%pairs_retained_df$set[pairs_retained_df$n_elements==(depth_level-1)]|search_df$X2%in%pairs_retained_df$set[pairs_retained_df$n_elements==(depth_level-1)],1,0)
      search_df$retain = base::ifelse(search_df$orig_variable_in_pair==1&search_df$highest_depth_in_pair==1,1,0)
      search_df = base::subset(search_df,search_df$retain==1)

      # Remove pairs with overlap (i.e., that an variable of the original data is also included in the variable of the previous depth level)
      search_df=base::subset(search_df,base::apply(search_df,1,function(x) base::grepl(paste0('\\b',x[1],'\\b'),base::strsplit(x[2],split=sep,fixed=T))==F))
    }

    # Calculate mutual information values of possible pairs
    search_df$mi_pairs = base::apply(search_df,1,function(x) mi(base::unlist(search_data[x[1]]),base::unlist(search_data[x[2]])))

    # Add the joint counts of the possible pairs
    search_df$joint_counts = base::apply(search_df,1,function(x) base::sum(base::unlist(search_data[x[1]])*(base::unlist(search_data[x[2]])),na.rm = T))

    # Calculate G statistic
    # If the percentage of joint counts is > 50% of the total number of observations, then the joint counts are flipped
    # Such that we apply the less frequent of the 0 vs. 1 effects to estimate power
    search_df$joint_counts_correct = base::ifelse(search_df$joint_counts > .5*(base::nrow(search_data)), base::nrow(search_data) - search_df$joint_counts, search_df$joint_counts)
    search_df$g = 2*(search_df$joint_counts_correct) * search_df$mi_pairs
    search_df$p = stats::pchisq(search_df$g, 1, lower.tail=FALSE)

    # Only keep the pairs which the user wants to retain
    if (base::is.null(MI.threshold)==T){
      search_df = base::subset(search_df,search_df$p<alpha)
    }
    else{
      search_df = base::subset(search_df,search_df$mi_pairs>MI.threshold)
    }

    # Stop if no pairs are retained
    if (nrow(search_df)==0){base::message(base::paste('stopped at max number of elements:',(depth_level-1)))
      break}

    # Add MI of variables of the previous depth level
    if (depth_level >2){
      if (depth_level==4){break}
      search_df = base::merge(x = search_df, y = dplyr::select(base::subset(pairs_retained_df,n_elements==(depth_level-1)),'mi','set'), by.x = 'X2',by.y = 'set', all.x = TRUE)
      search_df$mi_diff = search_df$mi_pairs - search_df$mi
    }
    else{
      search_df$mi_diff = 0
    }

    # Give pairs names in alphabetical order
    search_df$pair_names = base::paste(search_df$X1,search_df$X2,sep = sep)
    search_df$pair_names = base::apply(search_df['pair_names'], 1, function(x) base::paste(base::sort(base::strsplit(x,split=sep,fixed=T)[[1]]),collapse=sep))

    # Remove duplicates
    search_df=search_df[!base::duplicated(search_df$pair_names),]

    # Add retained pairs to retained pairs df
    current_rows=base::nrow(pairs_retained_df)
    pairs_retained_df[(current_rows+1):(current_rows+nrow(search_df)),'n_elements'] = depth_level
    pairs_retained_df[(current_rows+1):(current_rows+nrow(search_df)),'set'] = search_df$pair_names
    pairs_retained_df[(current_rows+1):(current_rows+nrow(search_df)),'mi'] = search_df$mi_pairs
    pairs_retained_df[(current_rows+1):(current_rows+nrow(search_df)),'relative.mi'] = search_df$mi_diff
    pairs_retained_df[(current_rows+1):(current_rows+nrow(search_df)),'p'] = search_df$p

    # Add pairs to the original data
    new_data = base::as.data.frame(base::apply(search_df,1,function(x) data[x[1]]*data[x[2]]))
    base::colnames(new_data)=search_df$pair_names
    data[,(base::ncol(data)+1):(base::ncol(data)+base::nrow(search_df))]=new_data

    # Update progress bar
    utils::setTxtProgressBar(pb,(depth_level-1))
  }

  # Close progress bar
  base::close(pb)

  # Create results list
  results_list = base::list()

  # Store the rest of the results
  results_list$expanded.data = data
  results_list$original.variables = orig_variables
  results_list$sets = pairs_retained_df

  # Return results
  return(results_list)
}
