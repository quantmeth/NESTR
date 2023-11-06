multilevel_corr <- function(.data, multilevel = NULL, ...){
  if(!exists(".data")) .data <- ...$.data
  R <- correlation::correlation(.data, multilevel = multilevel)

  list(.data,
       cormat = R,
       ... = ...)
  
}