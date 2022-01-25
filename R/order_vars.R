order_vars <- function(model){

  out <- network_path(model)
  vars <- out$vars
  p <- length(vars)

  # rearrangement of the table
  record = list()
  k = 0
  #while
  pos <- which(zero*upper.tri(zero) == 1, arr.ind=TRUE)
  nchange <- nrow(pos)
  while (nchange > 0) {
    for(i in 1:nchange){
      k = k + 1
      record[[k]] <- sort(vars[pos[i,]])
      if(any(duplicated(record))) {
        stop(paste(c("Model is non recursive with variables",
                     vars[pos[1]], "and",
                     vars[pos[2]]),
                   collapse = " "))
      }
      vars[pos[i,]] <- vars[pos[i,2:1]]
      zero <- zero[vars, vars]
    }
    pos <- which(zero*upper.tri(zero) == 1, arr.ind=TRUE)
    nchange <- nrow(pos)
  }
  out <- list(vars_order = colnames(zero),
              contingency_table = zero,
              DV = out$DV,
              exo = out$exo)
  return(out)
}
