# ajout de mle
rcfa <- function(R, factors, Lstart = .9){
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
