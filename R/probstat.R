#' probstat
#'
#' @description Computes marginal, conditional, and information-theoretic
#' summaries for a binary outcome `y` against one or more predictors in `x`.
#' Performs either Fisher's exact test or a generalized linear mixed model
#' (GLMM) for inference.
#'
#' @param y A binary outcome vector (logical or numeric coded as 0/1). Length
#'   `n`.
#' @param x A data frame of predictors (typically the expanded data returned by
#'   [pairmi()]). Must have `n` rows; columns are treated as candidate
#'   predictors.
#' @param test Character string selecting the inferential method; one of
#'   `c("fisher", "glmm")`. Defaults to `"fisher"` if missing.
#' @param ri Optional vector/factor giving the grouping variable for a random
#'   intercept in the GLMM. Must be length `n`. Ignored if `test = "fisher"`.
#' @param nfolds Integer; number of folds used for cross-validation.
#' @param seed Integer seed for fold randomization.
#'
#' @return A data frame with one row per evaluated predictor (or pair) and the
#'   following columns:
#' \describe{
#'   \item{xprob}{Marginal probability of \eqn{X=1}.}
#'   \item{yprob}{Marginal probability of \eqn{Y=1}.}
#'   \item{cprob}{Conditional probability \eqn{P(Y=1 \mid X=1)}.}
#'   \item{cprobx}{Conditional probability \eqn{P(X=1 \mid Y=1)}.}
#'   \item{cprobi}{Inverse conditional probability \eqn{P(Y=1 \mid X=0)}.}
#'   \item{cpdif}{Difference \eqn{P(Y=1 \mid X=1) - P(Y=1)}.}
#'   \item{cpdifper}{Percent difference relative to \eqn{P(Y=1)}.}
#'   \item{xent}{Entropy of \eqn{X}.}
#'   \item{yent}{Entropy of \eqn{Y}.}
#'   \item{ce}{Conditional entropy of \eqn{Y \mid X}.}
#'   \item{cedif}{Difference between marginal and conditional entropy of \eqn{Y}.}
#'   \item{cedifper}{Percent difference in entropy.}
#'   \item{p}{p-value from Fisher's exact test or the GLMM (as applicable).}
#' }
#'
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
