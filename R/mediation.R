#' @title Mediation Analysis
#'
#' @description Return all direct and indirect effects within a recursive path model.
#'
#' @param model an object of class \code{"formula"}: a symbolic description of the model to be fitted. More details of model specification are given under "Details". Basically, \code{model} is the order of the variables in the model from last (outcome variables) to first (exogenous variables).
#' @param data an optional data frame, list or environment (or object coercible by \code{as.data.frame()} to a data frame) containing the variables in the model. If \code{data = NULL}, the variables are taken from environment(formula), typically the environment from which \code{mediation} is called.
#' @param stat A function for the statistics to compute either \code{all_indirect} (saturated model) or \code{specific_indirect}
#' @param nrep Number of repetitions for the boostrap. Default is 5000.
#' @param alpha Type I error rate to build the \eqn{100 * (1-\alpha)}% confidence interval. Default is .05.
#' @param standardized Option to get standardized coefficients.
#'
#' @details Basically, \code{mediation} is a wrapper for the \code{\link[pathanalysis]{boot}} function from the  \code{pathanalysis} package.
#'
#' @return The mediation analysis
#'
#' @importFrom stats as.formula
#'
#' @export
#'
#' @examples
#'mediation(model = y ~ m2 ~ m1 ~ x, data = medEX, standardized = TRUE)

mediation <- function(model, data = NULL, stat = all_indirect, nrep = 5000, alpha = .05, standardized = FALSE){
  vars <- all.vars(model)
  if(is.null(data)){
    data <- sapply(FUN = get, X = vars, simplify = TRUE, envir = environment(model))
  }
  
  if(standardized){
    data <- apply(data, MARGIN = 2, FUN = function(x) (x-mean(x))/sd(x))
  }
  out <- list(results = boot(model = model, data = data, FUN = stat, nrep = nrep, alpha = alpha),
              model = model,
              alpha = alpha)
  results <- structure(out,
                       class = "PAmediation")
  
  return(results)
}
