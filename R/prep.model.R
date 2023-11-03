#' Prepare surrogate model for NEST
#'
#' @param R A correlation matrix.
#' @param factors The number of factors. 
#'
#' @return The surrogate model for NEST.
#' @export
#'
#' @examples
#' prep.model(R = ex_4factors_corr, factors = 2)
prep.model <- function(R, factors){
  # check if R is positive definite
  # Vérifier la sortie de RCFA
  # estimator = "rls" (if MLE is implemented)
  
  
  ###
  # Rr <- lapply(R, 
  #              FUN = {function(x, factors) {rcfa(x, factors)$Rr}}, 
  #              factors = factors)
  # Rc <- do.call("cbind", Rr)
  # or
  ###
  Rc <- rcfa(R, factors)$Rr
  pca <- svd(Rc)
  axes <- pca$u[,1:factors, drop = FALSE]
  # Vérifier polarité
  
  AX <- apply(axes, 2, FUN = function(x) c(x %*% t(x)))
  #model <- array(0, dim = c(ncol(R[[1]]), factors, length(R)))
  model <- list()
  ###
  # model$axes <- lapply(R, 
  #                 FUN = function(x, AX, axes){
  #                   D <- qr.solve(AX, c(x))
  #                   A <- axes %*% diag(sqrt(D), ncol = ncol(axes))
  #                   row.names(A) <- row.names(x)
  #                   A},
  #                 AX = AX, axes = axes)
  # model$model <- lapply(model$axes, function(x) x%*%t(x))
  # or
  ###
  D <- qr.solve(AX, c(R))
  A <- axes %*% diag(sqrt(D), ncol = ncol(axes))
  row.names(A) <- row.names(R)
  
  return(model = A %*% t(A))
  
}
