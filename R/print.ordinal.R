#' Print output of \code{dectect.ordinal()}
#' 
#' @description Print output of \code{dectect.ordinal()}.
#' @param x  An object of class "ordinal".
#' @param ... Further arguments for other methods, ignored for class "p.uniquevar".
#'
#' @author 
#' P.-O. Caron
#' @export
#'
#' @examples
#' set.seed(73)
#' mydata <- data.frame(Likert = sample(1:7, replace = TRUE, size = 20),
#'                      Bernouilli = sample(0:1, replace = TRUE, size = 20))
#' out <-  detect.ordinal(mydata)
#' print(out)
print.ordinal <- function(x, ...){
  nvar <- length(x$Variable)
  if(is.null(x$Variable)){
    cat("No variable appears to be ordinal.")
  } else if (nvar == 1) {
    cat(paste0("The variable ", names(x)," is probably ordinal.\nIt has less than ",max(unlist(lapply(x, length)))-1, " levels.\n"))
  } else {
    cat(paste0("The variables ", paste0(names(x), collapse = ", ")," are probably ordinal.\nThey have less than ",max(unlist(lapply(x, length)))-1, " levels."))
  }
}