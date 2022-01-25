#' @title Return a Specific Indirect Effect
#'
#' @description test test
#'
#' @param model test
#' @param data test
#'
#' @return test
#'
#' @export
#'
#' @importFrom stats as.formula coef lm
#'
#' @examples
#' all_indirect(model = y ~ m2 ~ m1 ~ x, data = medEX)

specific_indirect <- function(data, model){
  vars <- all.vars(model)
  p <- length(vars)
  est <- as.numeric()
  for (i in 1:(length(vars)-1)){
    mod <- as.formula(paste(vars[i],"~", paste(vars[(i+1):p], collapse = "+")))
    est[i] <- coef(lm(formula = mod, data = data))[vars[i+1]]
  }
  indirect_effect <- data.frame(Estimate = prod(est))
  rownames(indirect_effect) <- paste(vars[p:1], collapse = " -> ")
  return(indirect_effet = indirect_effect)
}

