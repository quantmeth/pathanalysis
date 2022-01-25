network_path <- function(model){
  
  out <- unlist_path(model)
  p <- length(out$vars)
  network.path <- matrix(0, p, p)
  colnames(network.path) <- rownames(network.path) <- vars <- c(out$exo, out$intermediary, out$outcome)
  
  for(i in 1:nrow(out$link)){
    network.path[out$link$DV[i], out$link$IV[i]] <- 1
  }
  
  out <- structure(append(list(association = network.path + t(network.path),
                               network.path = network.path),
                          out),
                   class = "pathanal")
  return(out)
}
