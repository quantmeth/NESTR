#' Print output of \code{p.unique.variable()}
#'
#' @description Print the results of \code{p.unique.variable()}
#' @param x An object of class "p.uniquevar".
#' @param ... Further arguments for other methods, ignored for class "p.uniquevar".
#'
#' @author 
#' P.-O. Caron
#' @export
#'
#' @examples
#' exData <- genr8(n = 420, R = ex_3factors_doub_unique)
#' out <- p.unique.variable(exData)
#' print(out)
print.puniquevar <- function(x, ...){
  out <- round(x$Results, 3)
  out$p[out$p == 0] <- paste("     < 0.001  ")
  out$p[out$p > .05] <- paste0("", out$p[out$p > .05]," *")
  colnames(out) <- c("F value","Pr(F>)   ")
  print(out)
}
