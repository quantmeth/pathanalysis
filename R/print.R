print.pathanal <- function(x){
  print(x[[1]])
  invisible(x)
}

# print.mediation <- function(x, ndigit = 3){
#   N <- attr(x,"row.names")
#   C <- sapply(x, simplify = "array", round, ndigit)
#   rownames(C) <- N
#   x$results <- C
#   print(C)
#   #print(round(x, 3))
#   invisible(x)
# }
