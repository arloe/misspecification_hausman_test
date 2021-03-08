#' The function that estimate p-value of hausman test
#' 
#' @importFrom MASS ginv
#' @importFrom stats pchisq
#' 
#' @param coef_marginal
#' @param coef_marginal
#' @param coef_marginal
#' @param coef_marginal

hausman_test <- function(coef_marginal, coef_condition, var_marginal, var_condition){
  # validate parameters
  if( length(x = coef_marginal) != length(x = coef_condition)) stop( "Length of 'coef_marginal', 'coef_condition' are different" )
  if( nrow(x = var_marginal) != ncol(x = var_marginal) ) stop(" 'var_marginal' must be a square matrix ")
  if( nrow(x = var_condition) != ncol(x = var_condition) ) stop(" 'var_condition' must be a square matrix ")
  if( nrow(x = var_marginal) != nrow(x = var_condition) ) stop( "Dimension of 'var_marginal', 'var_condition' are different" )
  
  # calculate degree of freedom
  degree = length(x = coef_marginal)
  # estimate statistics
  statistic = t(x = coef_marginal - coef_condition) %*% MASS::ginv(X = var_condition - var_marginal) %*% (coef_marginal - coef_condition)
  # calculate p-value
  p_value = pchisq(q = statistic, df = degree, lower.tail = FALSE)
  
  return( p_value )
}


#' The function that fit model using marginal MLE & conditional MLE
#' The response variable must be binary.
#' Marginal MLE is estimated using lme4 package & conditional MLE is estimated using survival package
#' 
#' @importFrom lme4 glmer
#' @importFrom survival clogit
#' 
#' @param formula_marginal The formula of marginal MLE (lme4 format)
#' @param formula_condition The formula of conditional MLE (survival format)
#' @param data the dataset

model_fit <- function(formula_marginal, formula_condition, data){
  # define parameters
  formula_marginal  = as.formula(object = formula_marginal)
  formula_condition = as.formula(object = formula_condition)
  
  # marginal MLE
  fit_marginal  = lme4::glmer(formula = formula_marginal, data = data, family = binomial(link = "logit"))
  coef_marginal = summary(object = fit_marginal)$coefficients[-1, 1]          # except intercept
  var_marginal  = as.matrix(x = summary(object = fit_marginal)$vcov)[-1, -1]  # except intercept 

  # conditional MLE
  library(survival)
  fit_condition  = survival::clogit(formula_condition, data = data)
  coef_condition = coef(object = fit_condition)
  var_condition  = fit_condition$var
  rownames(x = var_condition) = rownames(x = var_marginal)
  colnames(x = var_condition) = colnames(x = var_marginal)
  
  # return as list type
  result = list(  coef_marginal = coef_marginal, coef_condition = coef_condition
                , var_marginal = var_marginal, var_condition = var_condition)
  return(result)
}