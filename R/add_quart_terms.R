#' Creates quartile functional forms for all predictors
#' 
#' Assumes all predictors for given design matrix are continuous, and 
#' creates a matrix where every predictor has been modeled using quartiles
#' 
#' @param X design matrix of continuous predictors
#' 
#' @return design matrix where each predictor has been modeled using 
#' quartiles
#' 
#' @examples 
#' add_quart_terms(X)
#' 
#' @export 

add_quart_terms = function(X)
{
  x.new <- matrix(0,nrow=nrow(X),ncol=ncol(X))
  nonlin.count <- 0
  
  for(i in 1:ncol(X))
  {
    nonlin.count <- nonlin.count + 1
    quantiles <- as.vector(quantile(X[,i],probs=c(0.25,0.5,0.75)))
    
    # x1 indicator for quartile 2
    # x2 indicator for quartile 3
    # x3 indicator for quartile 4
    x1 <- as.numeric(X[,i]>quantiles[1] & X[,i]<=quantiles[2])
    x2 <- as.numeric(X[,i]>quantiles[2] & X[,i]<=quantiles[3])
    x3 <- as.numeric(X[,i]>quantiles[3])
    x.new[,(3*nonlin.count-2):(3*nonlin.count)] <- cbind(x1,x2,x3)
  }
  
  return(x.new)
}