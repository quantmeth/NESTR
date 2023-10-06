detect.unique.variable <- function(x){
  ndim <- dim(x)
  db <- ndim[1] - ndim[2] - 1
  R <- cor(x)
  B <- 1 / diag(solve(R))
  R2 <- 1 - B
  FF <- R2 * db / (B * ndim[2])
  p <- pf(FF, ndim[2], db, lower.tail = FALSE)                 
}