asym <- function(pt) {matrix(c(1,-2,1), ncol = 3) %*% pt / (matrix(c(-1,0,1), ncol = 3) %*% pt)}

# Test
ext = .05
data <- matrix(c(1:15, 30:16),15,2)

skew <- function(x){
  n <- length(x)      
  x <- x - mean(x)        
  sum(x^3) /       
    sqrt(sum(x^2))^3 
}

reSym <- function(data, alpha = .05) {
  # mon resym personel
  sk <- apply(data, 2, skew)
  se <- 2/sqrt(nrow(data));
  # inverse
  # check polarity
  data[,which(sk< -se)] <- -data[,which(sk < -se), drop = FALSE]   
  # check minimum
  data[,which(abs(sk)>se)] <- apply(data[,which(abs(sk)>se), drop = FALSE], 2, function(x) x-min(x)+1)
  # find best k to "un"skew
  data[,which(abs(sk)>se)] <- apply(data[,which(abs(sk)>se), drop = FALSE], 2, OPTSYM, alpha = alpha)
  # recheck skew
  sk2 <- apply(data, 2, skew)
  if(!any(abs(sk) >= abs(sk2))) cat("OH SHIT!")
  # repolarize
  data[,which(sk< -se)] <- -data[,which(sk < -se), drop = FALSE]   
  return(data)
  # TODO ADD OUTPUT, COMPUTE and return k,
}

OPTSYM <- function(x, alpha = .05){
  k <- optim(1, fn = optSym, method = "L-BFGS-B", x = x, alpha = alpha, lower=0)$par
  log(x+k)
}

optSym <- function(k, x, alpha = .05) {
  point <- quantile(x, probs = c(alpha, .50, 1-alpha))
  z <- log(point+k)
  -((z[1]-z[2])^2 + (z[2]-z[3])^2)
}


# 
# 
# reSym <- function(data, ext = .05){
#   cc <- .50
#   ndim <- dim(data)
#   # MSG?
#   # test .4 et 1
#   m0 <- colMeans(data)
#   sd0 <- apply(data, 2, sd)
#   data <- scale(data)
#   se <- 2/sqrt(ndim[1])
#   fractions <- c(ext, .5, 1-ext)
#   ff <- fractions * ndim[1] + cc
#   
#   A <- apply(data, 2, quantile, probs = fractions)
#   # interpolerang
#   #A <- data[ceiling(fractions * ndim[1]),]
#   #A[1,] <- colMeans(data[c(floor(ff[1]), ceiling(ff)[1]),])
#   #A[3,] <- colMeans(data[c(floor(ff[3]), ceiling(ff)[3]),])
#   #if(!ndim[1]%%2) A[2,] <- colMeans(data[c(floor(ff[2]), ceiling(ff)[2]),])
#   # voir pour les deux autres
#   
#   # A <- matrix(c(-1.5093,0,1.5093,-1.5093,0,1.5093),3,2)
#   
#   
#   
#   asym(A) # for each col
#   abs(asym(A)) > se
#   
#   par <- rep(NA, ndim[2])
#   datTr <- data
#   TI <- numeric()
#   
#   #  lapply(data, 2, )
#   #test
#   x <- data[,1]
#   SYM <- function(x){
#     as <- asym(x)
#   }
#   
# }
# 


