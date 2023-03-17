rpath <- function(n, 
                  Sigma = NULL,
                  Beta = NULL,
                  V = NULL,
                  mu = NULL, 
                  p = NULL, 
                  ...){
  
  if(!exists("n")){
    n <- 1
  }
  
  if(!is.null(Sigma) && !is.null(Beta)){
    stop("Choose either Beta or Sigma, not both")
  }
  
  if(!is.null(Beta) || !is.null(V)){
    if(is.null(Beta)) Beta <- matrix(0, length(V), length(V))
    Sigma <- beta2cov(B = Beta, V = V)
    p <- nrow(Sigma)
  }  
  
  if(is.null(Sigma) && is.null(mu)){
    if(is.null(p)){
      p <- 1
    } else {
      mu <- rep(0, p)
      Sigma <- diag(p)
    }
  } else if (!is.null(Sigma)){
    p <- nrow(Sigma)
    mu <- rep(0, p)
  } else if (!is.null(mu)){
    p <- length(mu)
    Sigma <- diag(p)  
  } 
  
  svdd <- svd(Sigma)
  ld <- svdd$u %*% diag(svdd$d^(.5))
  jd <- t(ld %*% matrix(stats::rnorm(n * p), p, n) + mu)
  colnames(jd) <- colnames(Sigma)
  
  return(jd)
}
