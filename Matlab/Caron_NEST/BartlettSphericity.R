BartlettSphericity <- function(R, n){
  p <- ncol(R)
  chisq <- -((n-1)-(2*p-5)/6)*log(det(R))
  df <- p*(p-1)/2
  p <- pchisq(chisq, df, lower.tail = FALSE)
  return(stat = list(chisq = chisq,
                     df = df,
                     p = p))
}
