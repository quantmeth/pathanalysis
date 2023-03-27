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
