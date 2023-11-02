#' Probability of unique variables
#'
#' @param x A data frame with only numeric variables.
#'
#' @return A data frame containing the F-values and probabilities of the variable to be an unique variable. 
#' @export
#'
#' @author 
#' P.-O. Caron (R)
#' André Achim (Matlab)
#' 
#' @examples
#' exData <- genr8(n = 420, R = ex_3factors_doub_unique)
#' p.unique.variable(exData)
p.unique.variable <- function(x){
  ndim <- dim(x)
  db <- ndim[1] - ndim[2] - 1
  R <- cor(x)
  B <- 1 / diag(solve(R))
  R2 <- 1 - B
  FF <- R2 * db / (B * ndim[2])
  p <- pf(FF, ndim[2], db, lower.tail = FALSE) 
  
  out <- data.frame(Fvalue = FF,
                    p = p)
  return(structure(list(Results = out), 
                   class = "puniquvar"))
}