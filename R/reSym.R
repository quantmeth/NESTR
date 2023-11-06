#' Resymmetrized data.
#'
#' @description TODO
#' @param x A data frame.
#' @param alpha A confidence interval for the symterization.
#'
#' @return A list of class "resym" containing the "un"skew data, the variables that were "un"skewed, the parameter of "un"skewness, the equation used to un"skew"
#' @export
#'
#' @examples
#' n <- 200
#' A <- rnorm(n)
#' B <- rlnorm(n, sd = 2)
#' C <- -rlnorm(n, sd = 2)
#' exData <- cbind(A,B,C)
#' round(apply(exData, 2, skew), 3)
#' reSym(exData)
reSym <- function(x, alpha = .05){
  sk <- apply(x, 2, skew)
  se <- 2/sqrt(nrow(x))
  
  # check polarity and reverse
  SIGN <- ifelse(sign(sk[which(abs(sk) > se)])>0,"","-")
  x[,which(sk < -se)] <- -x[,which(sk < -se), drop = FALSE]  
  
  # check minimum
  MIN <- apply(x[,which(abs(sk) > se), drop = FALSE],2,min)
  x[,which(abs(sk) > se)] <- apply(x[,which(abs(sk) > se), drop = FALSE], 2, function(x) x-min(x)+1)
  
  #   # find best k to "un"skew
  K <- apply(x[,which(abs(sk) > se), drop = FALSE], 2, OPTSYM, alpha = alpha, K = TRUE)
  
  apply(x[,which(abs(sk) > se), drop = FALSE], 2, OPTSYM, alpha = alpha, K = FALSE)
  
  x[,which(abs(sk) > se)] <- apply(x[,which(abs(sk) > se), drop = FALSE], 2, OPTSYM, alpha = alpha, K = FALSE)
  
  # recheck skew
  sk2 <- apply(x, 2, skew)
  
  if(!any(abs(sk) >= abs(sk2))) cat("OH SHIT!")
  
  # repolarize
  x[,which(sk < -se)] <- -x[,which(sk < -se), drop = FALSE]   

  eqn <- paste0(SIGN,
                "log(",
                SIGN,
                "x + ",
                round(MIN + 1 + K, 3),
                ")")
  
  sk2 <- (abs(sk) > se) * sk2
  sk2[sk2==0] <- NA
  SK <- data.frame(Original = sk,
                   Unskewed = sk2)
  
  OUT <- structure(list(.data = x,
              Variable = names(which(abs(sk) > se)),
              k = K,
              eqn = eqn,
              skewness = SK),
              class = "resym") 
  
  return(OUT)
}

OPTSYM <- function(x, alpha = .05, K = FALSE){
  k <- optim(1, fn = optSym, method = "L-BFGS-B", x = x, alpha = alpha, lower = -1)$par
  if(K){
    out <- k
  } else {
    out <- log(x+k)
  }
  return(out)
}

optSym <- function(k, x, alpha = .05) {
  point <- quantile(x, probs = c(alpha, .50, 1-alpha))
  z <- log(point+k)
  ((z[1]-z[2])^2  - (z[2]-z[3])^2) # square or not?
}
