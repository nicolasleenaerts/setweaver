#' pairmi
#' @description A function that calculates the mutual information for sets of variables, calculates the G statistic, determines the significance of the sets, and only keeps those that are significant.
#'
#' @param data A data.frame object with the variables that you want to pair up
#' @param alpha A numerical value indicating the p-value that is used to determine the significance of a set
#' @param MI.threshold A numerical value indicating the MI value that is used to determine the significance of a set Overrides the p-value
#' @param n_elements An integer indicating the maximum number of variables a set can have
#' @param sep A string which will be used to separate the variables in the pair name
#'
#' @return A list that includes
#' (1) A data.frame object with the original data and the data of the significant sets
#' (2) A vector with the original variable names
#' (3) A data.frame object with information on the significant sets
#' @export
#'
#' @examples
#' pairmi(misimdata[,2:6])
pairmi <- function(data,alpha=0.05,MI.threshold=NULL,n_elements=5,sep='_'){

  # Create df to store information about the pairs
  pairs_retained_df = data.frame(matrix(ncol=5))[-1,]
  colnames(pairs_retained_df) = c('n_elements','set','mi','relative.mi','p')

  # Get original variables
  orig_variables = colnames(data)

  # Create progress bar
  message('Pairing Data')
  pb = txtProgressBar(min = 0, max = (n_elements-1), initial = 0)

  # Create Pairs
  for (depth_level in 2:n_elements){

    # Reduce the number of variables to pair
    if (depth_level>2) {search_data=dplyr::select(data,c(dplyr::all_of(orig_variables),pairs_retained_df$set[pairs_retained_df$n_elements==(depth_level-1)]))
    } else {search_data=data}

    # Get all possible combinations of the variables
    search_df = data.frame(t(combn(colnames(search_data), 2, simplify=TRUE)))

    # Retain only correct combinations if depth level >2
    if (depth_level >2){

      # Only retain pairs with a variable of the original data and a variable of the previous depth level
      search_df$orig_variable_in_pair = ifelse(search_df$X1%in%dplyr::all_of(orig_variables)|search_df$X2%in%dplyr::all_of(orig_variables),1,0)
      search_df$highest_depth_in_pair = ifelse(search_df$X1%in%pairs_retained_df$set[pairs_retained_df$n_elements==(depth_level-1)]|search_df$X2%in%pairs_retained_df$set[pairs_retained_df$n_elements==(depth_level-1)],1,0)
      search_df$retain = ifelse(search_df$orig_variable_in_pair==1&search_df$highest_depth_in_pair==1,1,0)
      search_df = subset(search_df,retain==1)

      # Remove pairs with overlap (i.e., that an variable of the original data is also included in the variable of the previous depth level)
      search_df=subset(search_df,apply(search_df,1,function(x) grepl(paste0('\\b',x[1],'\\b'),strsplit(x[2],split=sep,fixed=T))==F))
    }

    # Calculate mutual information values of possible pairs
    search_df$mi_pairs = apply(search_df,1,function(x) mi(unlist(search_data[x[1]]),unlist(search_data[x[2]])))

    # Add the joint counts of the possible pairs
    search_df$joint_counts = apply(search_df,1,function(x) sum(unlist(search_data[x[1]])*(unlist(search_data[x[2]])),na.rm = T))

    # Calculate G statistic
    # If the percentage of joint counts is > 50% of the total number of observations, then the joint counts are flipped
    # Such that we apply the less frequent of the 0 vs. 1 effects to estimate power
    search_df$joint_counts_correct = ifelse(search_df$joint_counts > .5*(nrow(search_data)), nrow(search_data) - search_df$joint_counts, search_df$joint_counts)
    search_df$g = 2*(search_df$joint_counts_correct) * search_df$mi_pairs
    search_df$p = pchisq(search_df$g, 1, lower.tail=FALSE)

    # Only keep the pairs which the user wants to retain
    if (is.null(MI.threshold)==T){
      search_df = subset(search_df,p<alpha)
    }
    else{
      search_df = subset(search_df,mi_pairs>MI.threshold)
    }

    # Stop if no pairs are retained
    if (nrow(search_df)==0){message(paste('stopped at max number of elements:',(depth_level-1)))
      break}

    # Add MI of variables of the previous depth level
    if (depth_level >2){
      if (depth_level==4){break}
      search_df = merge(x = search_df, y = dplyr::select(subset(pairs_retained_df,n_elements==(depth_level-1)),'mi','set'), by.x = 'X2',by.y = 'set', all.x = TRUE)
      search_df$mi_diff = search_df$mi_pairs - search_df$mi
    }
    else{
      search_df$mi_diff = 0
    }

    # Give pairs names in alphabetical order
    search_df$pair_names = paste(search_df$X1,search_df$X2,sep = sep)
    search_df$pair_names = apply(search_df['pair_names'], 1, function(x) paste(sort(strsplit(x,split=sep,fixed=T)[[1]]),collapse=sep))

    # Remove duplicates
    search_df=search_df[!duplicated(search_df$pair_names),]

    # Add retained pairs to retained pairs df
    current_rows=nrow(pairs_retained_df)
    pairs_retained_df[(current_rows+1):(current_rows+nrow(search_df)),'n_elements'] = depth_level
    pairs_retained_df[(current_rows+1):(current_rows+nrow(search_df)),'set'] = search_df$pair_names
    pairs_retained_df[(current_rows+1):(current_rows+nrow(search_df)),'mi'] = search_df$mi_pairs
    pairs_retained_df[(current_rows+1):(current_rows+nrow(search_df)),'relative.mi'] = search_df$mi_diff
    pairs_retained_df[(current_rows+1):(current_rows+nrow(search_df)),'p'] = search_df$p

    # Add pairs to the original data
    new_data = as.data.frame(apply(search_df,1,function(x) data[x[1]]*data[x[2]]))
    colnames(new_data)=search_df$pair_names
    data[,(ncol(data)+1):(ncol(data)+nrow(search_df))]=new_data

    # Update progress bar
    setTxtProgressBar(pb,(depth_level-1))
  }

  # Close progress bar
  close(pb)

  # Create results list
  results_list = list()

  # Store the rest of the results
  results_list$expanded.data = data
  results_list$original.variables = orig_variables
  results_list$sets = pairs_retained_df

  # Return results
  return(results_list)
}