#' Print results of mediation
#'
#' @description Print the results of the mediation function.
#' @param x An object of class "PAmediation".
#' @param ... Further arguments for other methods, ignored for "mediation".
#'
#' @export
#'
#' @examples 
#' \dontrun{
#' results <- mediation(model = y ~ m2 ~ m1 ~ x, data = medEX, standardized = TRUE)
#' print(results)
#' }
print.PAmediation <- function(x, ...){
    round(x$results, 3)
  }
