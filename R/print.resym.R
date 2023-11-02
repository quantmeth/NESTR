#' Print output of \code{reSym()}
#'
#' @description Print the results of \code{p.reSym()}.
#' @param x An object of class "resymr".
#' @param ... Further arguments for other methods, ignored for classe "resym".
#'
#' @export
#'
#' @examples
#' n <- 200
#' A <- rnorm(n)
#' B <- rlnorm(n, sd = 2)
#' C <- -rlnorm(n, sd = 2)
#' x <- cbind(A,B,C)
#' round(apply(x, 2, skew), 3)
#' out <- reSym(x)
#' print(out)
print.resym <- function(x, ...){
  nvar <- length(x$Variable)
  if(nvar == 0){
    cat("No variable had to be resymmetrized.")
  } else {
    cat(paste0("The variable", .s(nvar)," ", paste0(x$Variable, collapse = ", ")," had to be resymmetrized."))
    for(i in 1:nvar){
      cat(paste0(
        " \n---\nThe variable ", x$Variable[i]," was resymmetrized by the equation \n",
        x$eqn[i], "\nwhich altered the original skewness of ", round(x$skewness[x$Variable[i],"Original"],3), " to ",  round(x$skewness[x$Variable[i],"Unskewed"],3),"."))
    }
  }
}

# .s ####
.s <- function(x, w = NULL){
  paste0(w, c("s")[x>1])
}
