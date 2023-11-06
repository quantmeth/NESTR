remove_unique <- function(.data, alpha = .05, ...){
  if(!exists(".data")) .data <- ...$.data
  .col <- apply(.data, 2, is.numeric)
  pval <- p.unique.variable(.data[,.col])$Results["p"]
  .rcol <- names(.col)[which(pval > alpha)]
  .data[,.rcol] <- NULL
  list(.data = .data,
       Variable = .rcol,
       p = pval)
}

