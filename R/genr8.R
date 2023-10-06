#' Simplify the the generation from a Multivariate Normal Distributions
#'
#' @description Speeds up the use of MASS::mvrnorm()
#'
#' @param n the number of samples required.
#' @param mean an optinal vector giving the means of the variables. Default is 0.
#' @param R a positive-definite symmetric matrix specifying the covariance matrix of the variables.
#' @param ... Argument of MASS::mvrnorm, such as \code{tol}, \code{empirical}, and \code{EISPACK}.
#'
#' @return A data frame of size n by ncol(R).
#' 
#' @author 
#' P.-O. Caron
#' 
#' @import MASS
#' @export
#'
#' @examples
#' set.seed(73)
#' mydata <- genr8(n = 10, R = ex_2factors, empirical = TRUE)
#' head(mydata)
#' cov(mydata)
#' 
genr8 <- function(n = 1, mean = rep(0, max(1, ncol(R))), R = diag(1), ...){
  as.data.frame(MASS::mvrnorm(n = n,
                              mu = mean,
                              Sigma = R,
                              ...))
}
