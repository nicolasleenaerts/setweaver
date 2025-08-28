#' probstat
#'
#' @param y Outcome (vector)
#' @param x Predictor (a data.frame object, typically the expanded dataset from
#'   the pairmi function)
#' @param test String indicating whether a Fisher's exact test or a Generalized
#'   linear mixed model needs to be performed (Integer)
#' @param ri Data of the variable that should be used as a random intercept in
#'   the Generalized linear mixed model (vector)
#' @param nfolds Number of folds for the cross-validation (Integer)
#' @param seed Number used for the randomization across folds (Integer)
#'
#' @return A data.frame object giving the following parameters for each pair:
#'   xprob = probability of X yprob = probability of Y cprob = conditional
#'   probability of Y given X cprobx = = conditional probability of X given Y
#'   cprobi = inverse conditional probability of Y given X cpdif = difference
#'   between marginal and conditional probability of Y cpdifper = percentagewise
#'   difference between marginal and conditional probability of Y xent = entropy
#'   of X yent = entropy of Y ce = conditional entropy cedif = difference
#'   between marginal and conditional entropy of Y cedifper = percentagewise
#'   difference between marginal and conditional entropy of Y p = p-value
#'   returned by the Fisher's exact test or Generalized linear mized model
#' @export
#' @examples
#' pairmiresult = pairmi(misimdata[,2:6])
#' probstat(misimdata$y,pairmiresult$expanded.data,nfolds=5)
probstat <- function(y, x, test='Fisher',ri,nfolds,seed=10101){

  # Split data into folds
  folds = splitTools::create_folds(y, k = nfolds, type='stratified', seed = seed)

  # Create results df
  dat = base::data.frame(matrix(ncol = 14, nrow = length(x)))

  # Set the column names for the results df
  base::colnames(dat) = c('rmse','xvars','yprob','xprob','cprob','cprobx','cprobi','cpdif',
                    'cpdifper','xent','yent','ce','cedif','cedifper')

  # Create placeholder variables
  rmses = base::c()
  dat_dfs = base::c()

  # Send message that the calculation of the entropy has begun
  base::message('Calculating entropy')

  # Loop over folds
  for (nfold in 1:base::length(folds)){

    # Calculate ce
    ent_in = entfuns(y[folds[[nfold]]], x[folds[[nfold]],])
    ent_out = entfuns(y[-folds[[nfold]]], x[-folds[[nfold]],])
    # Store RMSE
    rmses[nfold] = base::mean(base::sqrt((ent_in$ce - ent_out$ce)^2), na.rm = T)
    # Store calculations
    dat_dfs[[nfold]] = ent_in
    # Calculate the Odds Ratio between the presence/absence of X
    dat_dfs[[nfold]]$OR = base::round(base::ifelse(dat_dfs[[nfold]]$cprob == 1 | dat_dfs[[nfold]]$cprob == 0 | dat_dfs[[nfold]]$cprobi == 0, NA,
                                       (dat_dfs[[nfold]]$cprob/(1-dat_dfs[[nfold]]$cprob))/(dat_dfs[[nfold]]$cprobi/(1-dat_dfs[[nfold]]$cprobi))), digits = 2)
    # Calculate the Odds Ratio between the P(Y|X) and the marginal P(Y)
    dat_dfs[[nfold]]$ORmarg = base::round(base::ifelse(dat_dfs[[nfold]]$cprob == 1 | dat_dfs[[nfold]]$cprob == 0 | dat_dfs[[nfold]]$yprob == 0, NA,
                                           (dat_dfs[[nfold]]$cprob/(1-dat_dfs[[nfold]]$cprob))/(dat_dfs[[nfold]]$yprob/(1-dat_dfs[[nfold]]$yprob))), digits = 2)

  }

  # Aggregate dat_dfs
  dat =  dplyr::bind_rows(dat_dfs)
  dat =  stats::aggregate(dat[,-1],by = base::list(xvars=dat$xvars),mean,na.rm=T)

  # Send message that the calculation of the entropy has begun
  message('Performing statistical tests')
  
  # Calculate p value for OR
  if (test=='Fisher'){
    dat$p = base::apply(base::data.frame(dat$xvars),1,function(col){
      stats::fisher.test(table(y,factor(x[,col],levels=c(0:1))))$p.value
    }) 
  } else if (test == 'GLMM'){
    merged_df = base::cbind(outcome=y,random_intercept=ri,x)
    merged_df = merged_df[stats::complete.cases(merged_df),]
    dat$p = base::apply(base::data.frame(dat$xvars),1,function(colname){
      formula_string = base::paste0('outcome~`',colname,'`+(1|as.factor(random_intercept))')
      permutes::perm.glmer(stats::as.formula(formula_string),data=merged_df,family=stats::binomial(),nperm=1000)[2,5]
    }) 
  }
  
  # Clean up data
  dat[,3:16] <- base::round(dat[,3:16], digits = 3)
  dat$rmse = base::signif(mean(rmses,na.rm=T), digits = 3)

  # Order by CE
  dat <- dat[base::order(dat$ce),]

  # Return data
  return(dat)
}
