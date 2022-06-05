#' Creates quadratic functional forms for all predictors
#' 
#' Assumes all predictors for given design matrix are continuous, and 
#' creates a matrix where every predictor has been modeled using a quadratic 
#' term that does not include a linear term for each predictor
#' 
#' @param X design matrix of continuous predictors
#' 
#' @return design matrix where each predictor has been modeled using a quadratic 
#' term that does not include a linear component for each predictor
#' 
#' @examples 
#' add_quad_terms(X)
#' 
#' @export 
#' 
add_quad_terms = function(X)
{
  x.new <- matrix(0,nrow=nrow(X),ncol=ncol(X))
  nonlin.count <- 0
  
  for(i in 1:ncol(X))
  {
    nonlin.count <- nonlin.count + 1
    x.new[,nonlin.count] <- (X[,i])^2
  }
  
  return(x.new)  
}