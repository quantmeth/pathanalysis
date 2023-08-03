#' @title Compute a covariance matrix from regression coefficients and variances
#'
#' @description Compute a covariance matrix, \code{S}, from a lower triangular matrix containing regression coefficients (direct effects), \code{B} and the variances of the variables \code{V}.
#'
#' @param B A lower triangular matrix containing regression coefficients (direct effects).
#' @param V A vector containing the variance of the covariance matrix. If \code{V = NULL} (default), then \code{diag(S) = diag(p)} where \code{p} is the number of variables,
#'
#' @return A covariance matrix.
#' @export
#'
#' @examples
#' beta <- matrix(c( 0,  0,  0,
#'                  .4,  0,  0,
#'                  .2, .5,  0),
#'                  ncol = 3, nrow = 3, byrow = TRUE)
#' beta2cov(beta)
beta2cov = function(B, V = NULL){

  p = dim(B)[1]                 # Nombre de variables

  if(is.null(V)){               # Si V est nulle, alors V est une
    S = diag(p)                 # matrice diagonale d'identité,
  }else{                        # autrement il s'agit d'une matrice
    S = diag(V)                 # avec les variances en diagonale
  }
  
  colnames(S) <- colnames(B)
  rownames(S) <- rownames(B)
  
  # Boucle de calcul pour la covariance de la variable i (i = 2:p)
  for(i in 2:p){
    COV = B[(i), (1:(i-1))] %*% S[1:(i-1), 1:(i-1)]
    S[i, 1:(i-1)] = COV
    S[1:(i-1), i] = COV
  }

  return(S)

}
