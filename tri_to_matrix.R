tri_to_matrix <- function(x, include.diag=F){
  if (include.diag){
    num_datapoints <- length(x)
    num_orders <- sqrt((2*num_datapoints+0.25)) - 0.5
  } else {
    num_datapoints <- length(x)
    num_orders <- sqrt((2*num_datapoints+0.25)) + 0.5
  }
  covmatrix <- diag(0,num_orders)
  if (include.diag){
    covmatrix[lower.tri(covmatrix, diag=T)] <- x
  } else {
    covmatrix[lower.tri(covmatrix)] <- x
    diag(covmatrix) <- 1
  }
  covmatrix <- covmatrix + t(covmatrix)
  diag(covmatrix) <- diag(covmatrix)/2
  return(covmatrix)
}