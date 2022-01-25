print.pathanal <- function(x){
  print(x[[1]])
  invisible(x)
}

print.mediation <- function(x, ndigit = 3){
  print(round(x, ndigit))
  invisible(x)
}
