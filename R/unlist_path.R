unlist_path <- function(model){
  
  vars.type <- type_vars(model)
  
  link <- data.frame(matrix(0, nrow = length(vars.type$IV), ncol = 2))
  colnames(link) <- c("DV","IV")
  
  k = 0
  for(i in 1:length(model)){
    iv <- unlist(lapply(sapply(model[vars.type$endo==vars.type$endo[i]], "[[", 3), all.vars))
    for(j in 1:length(iv)){
      k = k + 1
      link[k,] <- c(vars.type$endo[i], iv[j])
    }
  }
  out <- structure(append(list(link = link), 
                          vars.type), 
                   class = "pathanal")
  return(out)
}
