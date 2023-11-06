ordinal <- function(.data, nOrdinal = 7, ...){
  
  if(!exists(".data")) .data <- ...$.data
  
  .col <- apply(.data, 2, is.numeric)
  
  stopifnot("Length of nOrdinal does not match the number of numeric variables or is not 1." = any(length(nOrdinal) == c(1,length(.col))))
  
  nOrd <- (apply(data, MARGIN = 2, FUN=function(x) sum(table(unique(x)))))
  var.ord <- names(which(nOrd <= nOrdinal))
  
  if(length(var.ord) == 0){
    var.ord <- NULL
  } else {
    Limits <- apply(data[var.ord], MARGIN = 2, ordinal.limits)
  }
  
  out <- list(.data = .data,
              Variable = var.ord,
              Ordinal.limits = Limits,
              ... = ...)
  
  return(structure(out, class = "ordinal"))
}
