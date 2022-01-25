#' @title Compute all regression coefficients from a covariance matrix
#'
#' @description Compute all direct regression coefficients (direct effects), \code{B} from a covariance matrix starting from the first variable to the last (order matter).
#' @param S A covariance matrix.
#'
#' @return A lower triangular matrix containing all direct effects (regression coefficients)
#' @export
#'
#' @examples
#' S <- matrix(c( 1, .2, .4,
#'               .2,  1, .3,
#'               .4, .5,  1), ncol = 3, nrow = 3)
#' cov2beta(S)
cov2beta = function(S){

  p = dim(S)[1]          # Nombre de variable
  BETA = matrix(0, p, p) # Matrice vide

  # Boucle de calcul pour la covariance de la variable i (i = 1:(p-1))
  for(i in 1:(p-1)){
    BETA[i+1, 1:i] = solve(S[1:i, 1:i], S[1+i, 1:i])
  }

  return(BETA)

}
