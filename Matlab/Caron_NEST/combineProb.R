combineProb <- function(probs){
  g <- ncol(probs)
  if(g > 1){
    z = rowSums(qnorm(probs))
    probs = pnorm(z/sqrt(g))
  }
  return(probs) 
}
