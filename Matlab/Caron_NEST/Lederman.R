Lederman <- function(p){
#ACHIM
    max.fact <- floor((2*p+1-sqrt(8*+p+1))/2)
  return(max.fact = max.fact)
}
max.fact <- function(nv){
#CARON
  dof <- function(nv, i = 0:nv){
    (nv-i)*(nv-i-1)/2 - i
  }
  max(which(dof(nv,0:nv)>(nv/2)))-1
}

