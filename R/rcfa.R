#' Regularized Common Factor Analysis
#'
#' @param R The correlation matrix.
#' @param factors The number of factors.
#' @param Lstart An initial value.
#'
#' @author 
#' P.-O. Caron (R)
#' André Achim (Matlab)
#'
#' @references
#' Jung, S. & Takane, Y. (2008). Regularized common factor analysis. \emph{New trends in psychometrics}, 141-149.
#'
#' @return A list of 
#' \itemize{
#'   \item \code{loadings} -A matrix of unrotated factor loadings.
#'   \item \code{Rr} - The reduced corrlation matrix.
#'   \item \code{Vb} - A vector of estimated communality values. 
#'   \item \code{L} - Value of the estimated penality parameter.

#' }
#' @import stats
#' @export
#'
#' @examples
#' rcfa(R = ex_4factors_corr, factors = 4)
rcfa <- function(R, factors, Lstart = .9){
  # AJOUT MLE ?
  Psi <- diag(1/diag(solve(R)))
  
  out <- optimize(Lstart, f = faSolution, lower = 0, upper = 1,
                  R = R, Psi = Psi, factors = factors, opt = TRUE)
  
  faSolution(L = out$minimum, R = R, Psi = Psi, factors = factors)
  
}  

# function to minimize
faSolution <- function(L, R, Psi, factors, opt = FALSE){
  Rr <- R - (L * Psi)
  UDU <- svd(Rr)
  U <- UDU$u
  D <- UDU$d
  axes <- U[,1:factors] %*% diag(sqrt(D[1:factors]), ncol = factors)
  if(opt){
    if(any(D<0) || any(rowSums(axes^2)>1)) {
      9e9
    } else {
      Rhat <- axes %*% t(axes)
      D <- axes %*% t(axes) - Rr
      axes <- sum(D^2)
    }
  } else {
    axes <- list(loadings = axes,
                 Rr = axes %*% t(axes),
                 Vb = diag(Psi),
                 L = L)
  }
  
  axes
  
}
