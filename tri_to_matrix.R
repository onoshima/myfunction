tri_to_matrix <- function(x){
  num_datapoints <- length(x)
  num_orders <- sqrt((2*num_datapoints+0.25)) + 0.5
  cormatrix <- diag(0,num_orders)
  cormatrix[lower.tri(cormatrix)] <- x
  cormatrix <- cormatrix + t(cormatrix)
  diag(cormatrix) <- 1
  return(cormatrix)
}