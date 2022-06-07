#' Creates cubic spline functional forms for all predictors
#' 
#' Assumes all predictors for given design matrix are continuous, and 
#' creates a matrix where every predictor has been modeled using a degree three 
#' B-spline basis with the bs() function
#' 
#' @param X design matrix of continuous predictors
#' 
#' @return design matrix where each predictor has been modeled using a degree 
#' three B-spline basis with the bs() function
#' 
#' @examples 
#' add_cubic_terms(X)
#' 
#' @export 

add_cubic_terms = function(X)
{
  X <- as.matrix(X)
  x.new <- matrix(0,nrow=nrow(X),ncol=ncol(X)*3)
  nonlin.count <- 0
  
  for(i in 1:ncol(X))
  {
    nonlin.count <- nonlin.count + 1
    x.new[,(3*nonlin.count-2):(3*nonlin.count)] <- bs(X[,i])
  }
  
  return(x.new)  
}