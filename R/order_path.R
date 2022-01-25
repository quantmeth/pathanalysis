model <- list(m ~ z, 
              q ~ m+y,
              y ~ m+x,
              z ~ x+a)

model <- list(m~z+b,
              z~x+a,
              q ~ w+m,
              w~y,
              y~m+x,
              f~g)
# (x + a + g + b) ~ z ~ m ~ y ~ w ~ (q + f)

order_path <- function(model, maxIter = "default"){
  out <- network_path(model)
  network.path <- out$network.path
  vars <- colnames(network.path)
  if(maxIter=="default") {
    maxIter <- choose(length(vars),2)
  }
  
  if(any(out$association > 1)){
    pos <- which(out$association > 1, arr.ind = TRUE)
    stop(paste(c("Model is non recursive with variables", 
                 vars[pos[1]], "and", 
                 vars[pos[2]]), 
               collapse = " "))
  }
  
  # rearrangement of the table
  record <- list()
  k <-  0
  pos <- which(network.path*upper.tri(network.path) == 1, arr.ind = TRUE)
  nchange <- nrow(pos)
  while (nchange > 0) {
    
    if(k == maxIter){
      stop(paste(c("Model do not converge toward an order")))
    }
    
    k = k + 1
    record[[k]] <- sort(vars[pos[1,]])
    if(any(duplicated(record))) {
      stop(paste(c("Model is probably non recursive with variables", 
                   vars[pos[1]], "and", 
                   vars[pos[2]]), 
                 collapse = " "))
    }
    vars[pos[1,]] <- vars[pos[1, 2:1]]
    network.path <- network.path[vars, vars]
    pos <- which(network.path * upper.tri(network.path) == 1,
                 arr.ind=TRUE)
    nchange <- nrow(pos)
  }
  network.path <- network.path[vars, vars]
  out <- structure(list(path.order = colnames(network.path), 
                        path.network = out$association), 
                   class = "pathanal")
  return(out)
}
