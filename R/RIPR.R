#' Runs the RIPR algorithm
#' 
#' Runs the RIPR algorithm/performs the appropriate ridge regression fit using
#' the updated design matrix including linear, quadratic, quartile, and cubic
#' functional forms for the continuous predictors 
#' 
#' @param X data frame with all predictors of interest. Categorical and
#' continuous predictors can be included, where the class of the categorical 
#' predictors must be factors, and the class of the continuous predictors
#' must be numeric. 
#' @param Y response vector with length equal to the number of rows of X
#' 
#' @return two-item list where the first item is the updated design matrix
#' used for the ridge regression fit of the RIPR algorithm, and the second item
#' is glmnet object for the ridge regression fit 
#' 
#' @examples 
#' RIPR(X,Y)
#' 
#' @export 
#' 
RIPR = function(X,Y)
{
  X_RIP <- build_RIPR_mat(X)
  ridge_fit <- cv.glmnet(x=data.matrix(X_RIP),y=as.vector(Y),
                         alpha=0,type.measure="mse")
  
  return(list(X_RIP,ridge_fit)) 
}