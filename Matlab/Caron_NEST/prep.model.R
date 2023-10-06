#factors <- 2
prep.model <- function(R, factors, estimator = "rls"){
  # check if R is positive definite
  ## Vérifier la sortie de RCFA
  Rr <- lapply(R, 
               FUN = {function(x, factors) {rcfa(x, factors)$Rr}}, 
               factors = factors)
  Rc <- do.call("cbind", Rr)
  pca <- svd(Rc)
  axes <- pca$u[,1:factors, drop = FALSE]
  # Vérifier polarité
  
  AX <- apply(axes, 2, FUN = function(x) c(x %*% t(x)))
  #model <- array(0, dim = c(ncol(R[[1]]), factors, length(R)))
  model <- list()
  # using pracma
  # pracma::mldivide(AX, c(R[[1]]))
  # base R
  model$axes <- lapply(R, 
                  FUN = function(x, AX, axes){
                    D <- qr.solve(AX, c(x))
                    A <- axes %*% diag(sqrt(D), ncol = ncol(axes))
                    row.names(A) <- row.names(x)
                    A},
                  AX = AX, axes = axes)
  model$model <- lapply(model$axes, function(x) x%*%t(x))
  return(model)
}
