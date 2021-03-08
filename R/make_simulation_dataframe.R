#' A function that generate simulation dataset
#' 
#' @importFrom stats rnorm rgamma rbinom runif
#' 
#' @param N the number of person
#' @param n the number of repetition
#' @param family the distribution of random effect
#'
#' @references Tchetgen, E. J. and Coull, B. A. (2006). 
#'             A diagnostic test for the mixing distribution in a generalised linear mixed model. 
#'             Biometrika, 93:1003–1010.

make_simulation_dataframe <- function(N, n, family){
  # validate parameters
  if( !family %in% 1:8 ) stop("'family' must be a nature number between 1 and 8")
  
  # define true beta coefficients
  true_beta = c( -.5, .2, .5 )
  
  # generate simulation data
  df = data.frame(  id   = integer(length = N*n)
                  , time = integer(length = N*n)
                  , y    = integer(length = N*n)
                  , X    = integer(length = N*n) )
  for( i in 1:N ){
    # define id and time
    id   = i
    time = 1:n
    
    # generate covariates
    x = rbinom(n = 1, size = 1, prob = 0.5)
    X = rep(x, length = n)
    
    # determine the distribution of random effect
    # reference: A diagnostic test for the mixing distribution in a generalised linear mixed model. 
    if(family == 1) v = rnorm(n = 1, mean = 0, sd = 1)                # Standard Normal
    if(family == 2) v = 2*rgamma(n = 1, shape = 1, scale = 1)         # Gamma 1
    if(family == 3) v = 3*rgamma(n = 1, shape = 0.5, scale = 1)       # Gamma 0.5
    if(family == 4) v = rnorm(n = 1, mean = 0, sd = 1 + 2*x)              # Heterogeneous Variance 1
    if(family == 5) v = rnorm(n = 1, mean = 0, sd = 0.5 + 2.5*x)          # Heterogeneous Variance 0.5
    gamma_param = rbinom(n = 1, size = 1, prob = 0.3)
    if(family == 6) v = gamma_param*rnorm(n = 1, mean = 2, sd = sqrt(0.5)) + 
      (1 - gamma_param) * rnorm(n = 1, mean = -0.86, sd = sqrt(0.5))  # Mixture 0.5
    if(family == 7) v = gamma_param*rnorm(n = 1, mean = 2, sd = sqrt(0.25)) + 
      (1 - gamma_param) * rnorm(n = 1, mean = -0.86, sd = sqrt(0.25)) # Mixture 0.25
    if(family == 8) v = runif(n = 1, min = -sqrt(6), max = sqrt(6))       # Uniform
    
    # calculate independent, response variable
    Xmat = cbind(1, time, time*X)
    z    = Xmat %*% true_beta + v
    prob = 1 / (1 + exp(-z))
    y    = rbinom(n = n, size = 1, prob = prob)
    
    # combine
    df[(n*(i-1)+1):(n*i), ] = cbind(id, time, y, X)
  }

  return(df)
}
