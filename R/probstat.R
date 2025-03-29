#' probstat
#'
#' @param y Outcome (vector)
#' @param x Predictor (a data.frame object, typically the expanded dataset from the pairmi function)
#' @param nfolds Number of folds for the cross-validation (Integer)
#' @param seed Number used for the randomization across folds (Integer)
#'
#' @return A data.frame object giving the following parameters for each pair:
#' xprob = probability of X
#' yprob = probability of Y
#' cprob = conditional probability of Y given X
#' cprobx = = conditional probability of X given Y
#' cprobi = inverse conditional probability of Y given X
#' cpdif = difference between marginal and conditional probability of Y
#' cpdifper = percentagewise difference between marginal and conditional probability of Y
#' xent = entropy of X
#' yent = entropy of Y
#' ce = conditional entropy
#' cedif = difference between marginal and conditional entropy of Y
#' cedifper = percentagewise difference between marginal and conditional entropy of Y
#' @export
#' @examples
#' probstat(misimdata$y,pairmiresult$expanded.data,5)
probstat <- function(y, x, nfolds,seed=10101){

  # Set seed to ensure same results
  set.seed(seed)

  # Split data into folds
  folds = splitTools::create_folds(y, k = nfolds, type='stratified', seed = seed)

  # Create results df
  dat = data.frame(matrix(ncol = 14, nrow = length(x)))

  # Set the column names for the results df
  colnames(dat) = c('rmse','xvars','yprob','xprob','cprob','cprobx','cprobi','cpdif',
                    'cpdifper','xent','yent','ce','cedif','cedifper')

  # Create placeholder variables
  rmses = c()
  dat_dfs = c()

  # Create progress bar
  message('Calculating entropy')
  pb = txtProgressBar(min = 0, max = (nfolds), initial = 0)

  # Loop over folds
  for (nfold in 1:length(folds)){

    # Calculate ce
    ent_in = entfuns(y[folds[[nfold]]], x[folds[[nfold]],])
    ent_out = entfuns(y[-folds[[nfold]]], x[-folds[[nfold]],])
    # Store RMSE
    rmses[nfold] = mean(sqrt((ent_in$ce - ent_out$ce)^2), na.rm = T)
    # Store calculations
    dat_dfs[[nfold]] = ent_in
    # Calculate the Odds Ratio between the presence/absence of X
    dat_dfs[[nfold]]$OR = round(ifelse(dat_dfs[[nfold]]$cprob == 1 | dat_dfs[[nfold]]$cprob == 0 | dat_dfs[[nfold]]$cprobi == 0, NA,
                                       (dat_dfs[[nfold]]$cprob/(1-dat_dfs[[nfold]]$cprob))/(dat_dfs[[nfold]]$cprobi/(1-dat_dfs[[nfold]]$cprobi))), digits = 2)
    # Calculate the Odds Ratio between the P(Y|X) and the marginal P(Y)
    dat_dfs[[nfold]]$ORmarg = round(ifelse(dat_dfs[[nfold]]$cprob == 1 | dat_dfs[[nfold]]$cprob == 0 | dat_dfs[[nfold]]$yprob == 0, NA,
                                           (dat_dfs[[nfold]]$cprob/(1-dat_dfs[[nfold]]$cprob))/(dat_dfs[[nfold]]$yprob/(1-dat_dfs[[nfold]]$yprob))), digits = 2)

    # Update progress bar
    setTxtProgressBar(pb,(nfold))
  }

  # Close progress bar
  close(pb)

  # Aggregate dat_dfs
  dat =  bind_rows(dat_dfs)
  dat =  aggregate(dat[,-1],by = list(xvars=dat$xvars),mean,na.rm=T)

  # Calculate p value for OR
  dat$p = apply(x,2,function(x)
    fisher.test(table(y,factor(x,levels=c(0:1))))$p.value
  )

  # Clean up data
  dat[,3:16] <- round(dat[,3:16], digits = 3)
  dat$rmse = signif(mean(rmses,na.rm=T), digits = 3)

  # Order by CE
  dat <- dat[order(dat$ce),]

  # Return data
  return(dat)
}
