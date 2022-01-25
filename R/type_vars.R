type_vars <- function(model){
  out <- list()
  if(!is.list(model)){model <- list(model)}
  l.vars <- unlist(lapply(model, all.vars))
  out$vars <- unique(l.vars)
  IV <- unlist(lapply(sapply(model, "[[", 3), all.vars))
  out$endo <- unlist(lapply(sapply(model, "[[", 2), all.vars))
  out$exo <- out$vars[!(out$vars %in% out$endo)]
  out$outcome <- out$vars[!(out$vars %in% IV)]
  out$intermediary <- out$vars[!(out$vars %in% c(out$outcome, out$exo))]
  out$IV <- IV
  out <- structure(out, class = "pathanal")
  return(out)
}
