#' @title Bootstrap
#'
#' @description A generic function to boostrap the desired statistics.
#'
#' @param data The data sets.
#' @param FUN A function comuting the desired statistics.
#' @param alpha Type I error rate. Default is .05.
#' @param nrep Number of repetitions for the boostrap. Default is 5000.
#' @param ... Optional arguments for \code{FUN}.
#'
#' @return Return the estimates computed by \code{FUN}, their standard error and their \eqn{100 * (1-\alpha)}% confidence interval.
#'
#' @export
#'
#' @importFrom stats sd pnorm quantile
#'
#' @examples
#' boot(data = rpois(n = 10, lambda = 5), FUN = median, nrep = 10)
boot <- function(data, FUN, alpha = .05, nrep = 5000, ...){
  data <- as.matrix(data)
  p <- ncol(data) # Number of variables
  n <- nrow(data) # Number of subjects

  # Empty matrix to record estimators with the 1st column
  # being the actual estimators
  e <- FUN(data, ...)
  Est <- data.frame(Est = e,
                    nrep = matrix(0, ncol = nrep))

  # The loop
  # Starting at 2 given the 1st column
  for(i in (1:nrep)+1){
    index <- sample(n, replace = TRUE)
    D <- data[index,]
    Est[,i] <- FUN(D, ...)
  }

  # Put everything together
  SE = apply(Est, MARGIN = 1, FUN = sd)
  Results <- data.frame(
    Estimate = Est$Est,
    SE,
    apply(Est, MARGIN = 1, FUN = quantile, probs = alpha/2),
    apply(Est, MARGIN = 1, FUN = quantile, probs = 1 - alpha/2),
    (1 - pnorm(abs(Est$Est/SE)))*2
  )
  colnames(Results) <- c("Estimate",
                          "S.E.",
                          paste("CI Lower",(1-alpha)*100,"%"),
                          paste("CI Upper",(1-alpha)*100,"%"),
                          "p-value")
  return(Results)
}

