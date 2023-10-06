unique.var <- function(data, time.var = NULL, alpha = .05){
  if(is.null(time.var)){
    data$time.var <- 1
    time.var <- "time.var"
  }
  ndim <- c(dim(data[,-1]), dim(unique(data[time.var]))[1])
  # pr <- matrix(0, ndim[2], ndim[3])
  db <- ndim[1]-ndim[2]-1
  p0 <- by(data[,!names(data) %in% time.var], 
           INDICES = data[,time.var], 
           FUN = check.unique.var)
  p <- simplify2array(p0)
  
  if(ndim[3] > 1){
    p <- combineProb(p)
  }
  
  uni <- which(p > alpha)
  
  if(length(uni) == 0){uni <- NULL}
  
  return(list(unique.variables = uni, 
              probs = p))
}

check.unique.var <- function(x){
  ndim <- dim(x)
  db <- ndim[1] - ndim[2] - 1
  R <- cor(x)
  B <- 1 / diag(solve(R))
  R2 <- 1 - B
  FF <- R2 * db / (B * ndim[2])
  p <- pf(FF, ndim[2], db, lower.tail = FALSE)                 
}
