#' @title test
#'
#' @description test test
#'
#' @param data ggafadfb
#' @param model test
#'
#' @return All indirect effets in a data from the first independent variable to the last outcome.
#'
#' @export
#'
#' @importFrom utils combn
#' @importFrom stats cov
#'
#'
#' @examples
#' all_indirect(model = y ~ m1 ~ x, data = medEX)
all_indirect <- function(data, model){
  # A function estimating all directs and indirects
  vars <- all.vars(model)
  p <-  length(vars)
  vars <- vars[p:1]
  ndata <- data[,vars]
  COV <- cov(ndata)

  # Compute regression coefficients from
  # covariance matrix
  BETA <- cov2beta(COV)

  # Record all estimates
  est <- as.matrix(BETA[lower.tri(BETA)])

  # Labels all direct effects
  label <- matrix(vars[combn(p, 2)], (p * (p-1) / 2), 2, byrow = TRUE)
  rname <- apply(FUN = paste, as.matrix(label[,1]), MARGIN = 2, "->")
  rname <- apply(FUN = paste, rname, label[,2], MARGIN = 2, "")
  row.names(est) <- rname

  # List all possibles indirect effects
  if(p != 3){
    # If there is more than 3 variables, list all indirect effects by their level
    listeffects <- mapply(combn, p, 3:p)
  } else {
    # If there is 3 variables, list the single indirect effect at its level
    listeffects <- list((matrix(1:3, 3, 1)))
  }

  # Compute all indirect effects and their labels
  for(i in 1:length(listeffects)){ # Number of levels
    J <- ncol(listeffects[[i]])    # Number of indirect effects at this level
    for(j in 1:J){
      ide <- listeffects[[i]][,j]  # Target indirect effect
      B <- BETA[ide, ide]          # Their coefficients
      B <- B[-1, -ncol(B)]         # Remove unneeded coefficients
      e <- as.matrix(prod(diag(B)))   # Compute the indirect effet
      rownames(e) <- paste(vars[ide], # Its label
                           collapse = " -> ")
      est <- rbind(est, e)         # Add to other indirect effects.
    }
  }

  # Add total effects
  totald <- as.matrix(solve(COV[1, 1], COV[p, 1]))
  totali <- as.matrix(totald - BETA[p, 1])

  # Add label for total indirect and total effect
  rownames(totali) <- paste("total indirect",
                            vars[1],
                            "->",
                            vars[p])
  rownames(totald) <- paste("total effect",
                            vars[1],
                            "->",
                            vars[p])

  # Put everything together
  estimates <- rbind(est, totali, totald)
  return(estimates)
}
