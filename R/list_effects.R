list_effects <- function(p){
  if(p != 3){
    # If there is more than 3 variables, list all indirect effects by their level
    listeffects <- mapply(combn, p, 3:p)
  } else {
    # If there is 3 variables, list the single indirect effect at its level
    listeffects <- list((matrix(1:3, 3, 1)))
  }
  return(listeffects)
}
