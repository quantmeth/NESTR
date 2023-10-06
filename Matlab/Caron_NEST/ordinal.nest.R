# FUNCTION
detect.ordinal <- function(data, nOrdinal = 10){
  nOrd <- (apply(data, MARGIN = 2, FUN=function(x) sum(table(unique(x)))))
  var.ord <- names(which(nOrd <= nOrdinal))
  if(length(var.ord) == 0){
    var.ord <- NULL
  } else {
    var.ord <- apply(data[var.ord], MARGIN = 2, ordinal.limits)
  }
  return(var.ord)
}

ordinal.limits <- function(variable){
  c(-Inf, qnorm(cumsum(table(variable)) / length(variable)))
}

replace.ordinal <- function(x, D, ord.var) {
  apply(D[x], MARGIN = 1,
         FUN = function(z, x, ord.var) {
           as.numeric(names(ord.var[[x]])[min(which(z < ord.var[[x]]))])
           },
         x = x, ord.var = ord.var)
}
