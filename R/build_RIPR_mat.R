#' Builds matrix for RIPR algorithm
#' 
#' Takes design matrix and creates updated design matrix where each continuous
#' predictor has been modeled with linear, quadratic, quartile, and cubic
#' functional forms 
#' 
#' @param X data frame of predictors, which can include both continous and 
#' categorical predictors. Categorical predictors must be of class factor
#' and continuous predictors must be of class numeric
#' 
#' @return updated design matrix where each continuous predictor has been 
#' modeled using linear, quadratic, quartile, and cubic functional forms.
#' All categorical predictors appear at the end of this updated design matrix, 
#' after all functional forms of the continuous predictors
#' 
#' 
#' @examples 
#' build_RIPR_mat(X)
#' 
#' @export 
#' 
build_RIPR_mat = function(X)
{
  # Keep track of which predictors are continuous/categorical
  # Let 0 indicate continuous predictor and 1 indicate categorical predictor
  col.types <- rep(0,ncol(X))
  
  # Separate continuous and categorical predictors 
  for(i in 1:ncol(X)){if(class(X[,i])=="factor"){col.types[i] <- 1}}
  
  X.cat <- X[,which(col.types==1)]
  X.con <- X[,which(col.types==0)]
  
  cat.col.names <- colnames(X.cat)
  cont.col.names <- colnames(X.con)
  
  # Generate matrices of quadratic, quartile, and cubic spline functional forms
  # for continuous predictors
  x.quad <- add_quad_terms(X.con)
  x.quart <- add_quart_terms(X.con)
  x.cubic <- add_cubic_terms(X.con)
  
  # Matrix with all functional forms for continuous predictors only
  X.con.final <- cbind(X.con,x.quad,x.quart,x.cubic)
  
  # Append categorical predictors to the end of design matrix
  X.final <- cbind(X.con.final,X.cat)
  
  # Assign all predictors in updated design matrix with original names for 
  # Original continuous/categorical predictors, and for the new functional forms
  # of continuous predictors, provide the functional form and which basis 
  # component for the quartiles and cubic spline functional forms 
  quad.names <- sapply(cont.col.names, function(x) paste(x, "quad", sep=""))
  quart1.names <- sapply(cont.col.names, function(x) paste(x, "quart2", sep=""))
  quart2.names <- sapply(cont.col.names, function(x) paste(x, "quart3", sep=""))
  quart3.names <- sapply(cont.col.names, function(x) paste(x, "quart4", sep=""))
  
  all.cont.col.names <- rep("",ncol(X.con)*8)
  
  all.cont.col.names[1:ncol(X.con)] <- cont.col.names
  all.cont.col.names[(ncol(X.con)+1):(2*ncol(X.con))] <- quad.names
  
  quart.col.names <- rep("",ncol(X.con)*3)
  
  for(i in 1:(3*ncol(X.con)))
  {
    cur.col.name.quart <- ""
    
    if(i %% 3 == 1){cur.col.name.quart <- quart1.names[i %/% 3 + 1]} 
    else if(i %% 3 == 2){cur.col.name.quart <- quart2.names[i %/% 3 + 1]}
    else{cur.col.name.quart <- quart3.names[i %/% 3]}
    
    quart.col.names[i] <- cur.col.name.quart
  }
  
  spline_col_names <- quart.col.names
  spline_col_names <- stringr::str_replace_all(spline_col_names, "quart2" ,"spline1")
  spline_col_names <- stringr::str_replace_all(spline_col_names, "quart3" ,"spline2")
  spline_col_names <- stringr::str_replace_all(spline_col_names, "quart4" ,"spline3")
  
  all.cont.col.names[(2*ncol(X.con)+1):length(all.cont.col.names)] <- 
    c(quart.col.names,spline_col_names)
  
  colnames(X.final) <- c(all.cont.col.names,cat.col.names)
  return(X.final)
}