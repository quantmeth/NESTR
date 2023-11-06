symmetrize <- function(.data, alpha = .05, ...) {
  if(!exists(".data")) .data <- ...$.data
  .col <- apply(.data, 2, is.numeric)
  #reSym(.data, alpha = alpha)
  reSym(.data, alpha = alpha)

  #out[c("x","Variable")]
  
  # nOrdinal = 10
  #if(!any(length(nOrdinal) == c(1,length(.col)))) 
  #stopifnot("Length of nOrdinal does not match the number of numeric variables." = any(length(nOrdinal) == c(1,length(.col))))
  
  #out <- detect.ordinal(.data[,.col])
  #out
}