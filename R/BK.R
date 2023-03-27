#' The causal step methods more widely known as the Baron-Kenny method
#' @description Perform the Baron-Kenny method on a recursive path model.
#' @param out An output of class \code{PAmediaiton} form pathanalysis::mediation.
#' @param alpha Type I error rate. Default is the one from the original mediation.
#'
#' @return Determine if there is a mediated relation for each indirect effects and whether it is partial or complete.
#' @export
#'
#' @examples
#' res <- mediation(model = y ~ m2 ~ m1 ~ x, data = medEX, standardized = TRUE)
#' BK(res)
BK <- function(out, alpha = out$alpha){
  model <- out$model
  res <- out$results
  vars <- all.vars(model)
  p <- length(vars)
  
  listeffects <- list_effects(p = p)
  n.ind <- sum(sapply(listeffects, ncol))
  n.dir <- (choose(p, 2))
  
  res["total effect", "p-value"] <= alpha
  
  PV <- matrix(0, ncol = p, nrow = p, dimnames = list(vars[p:1],
                                                      vars[p:1]))
  PV[upper.tri(PV)] <- res[1:n.dir, "p-value"]
  
  out <- as.data.frame(t(matrix(unlist(sapply(X = listeffects, 
                                              FUN = function(X) {apply(X = X, 
                                                                       MARGIN = 2,
                                                                       FUN = function(x) {
                                                                         c(BK = as.logical(prod((PV[cbind(row = x[-length(x)], 
                                                                                                          col = x[-1])]) <= alpha)),
                                                                           Type = as.logical((PV[x[1],x[length(x)]] > alpha)))
                                                                       })})), nrow = 2, ncol = n.ind, 
                                dimnames = list(c("BK","Type"), row.names(res)[n.dir+1:n.ind]))))
  out$Conclusion <- paste0(ifelse(!out$BK, "No", ifelse(out$Type, "Complete", "Partial"))," mediation")
  return(out)
} 
