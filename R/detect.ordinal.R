#' Detect potential ordinal variable.
#' 
#' @description Detect potential ordinal variables and compute the dentisity of each level on a gaussian distribution which can be taken into account for sampling techniques like \code{replace.ordinal()}.
#'
#' @param data A data set.
#' @param nOrdinal The minimal of values allowed before being flags as ordinal. Default is 10.
#' 
#' @author 
#' André Achim (Matlab)
#' P.-O. Caron (R)
#' 
#' @return A list of detected ordinal variables and the density of each value.
#' 
#' @references 
#' À ajouter.
#' 
#' @import stats
#' @export
#'
#' @examples
#' set.seed(73)
#' mydata <- data.frame(Likert = sample(1:7, replace = TRUE, size = 20),
#'                      Bernouilli = sample(0:1, replace = TRUE, size = 20))
#' detect.ordinal(mydata)
detect.ordinal <- function(data, nOrdinal = 10){
  nOrd <- (apply(data, MARGIN = 2, FUN=function(x) sum(table(unique(x)))))
  var.ord <- names(which(nOrd <= nOrdinal))
  if(length(var.ord) == 0){
    var.ord <- NULL
  } else {
    var.ord <- apply(data[var.ord], MARGIN = 2, ordinal.limits)
  }
  return(structure(var.ord, class = "ordinal"))
}

ordinal.limits <- function(variable){
  c(-Inf, qnorm(cumsum(table(variable)) / length(variable)))
}
