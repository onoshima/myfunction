cor_to_cov <- function(R, SD){
  if (nrow(R)!=ncol(R)){
    stop("The input matrix is not squared.")
  } else if (nrow(R)!=length(SD)){
    stop("The number of variables of correlation matrix and SD vector are dirfferent.")
  }
  D <- diag(SDs)
  V <- D %*% R %*% D
  rownames(V) <- rownames(R)
  colnames(V) <- colnames(R)
  return(V)
}