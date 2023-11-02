#' Calculate univariate skew of a variaable
#'
#' @param x A vector
#'
#' @return The skewness of the variable.
#' @export
#'
#' @examples
#' set.seed(42)
#' x <- rnorm(420)^2
#' skew(x)
skew <- function(x){
  n <- length(x)      
  x <- x - mean(x)        
  sum(x^3) /       
    sqrt(sum(x^2))^3 
}