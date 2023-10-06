#'  Convert continuous data into ordinal data
#'
#' @description Using the density of each level, convert a continuous variables into an ordinal variable. Useful with \code{detect.ordinal()}.
#'
#' @param ord.var A data frame with ordinal variables' name as column names and the probability of each ordinal values in the row.
#' @param D A data frame to be converted (normal to ordinal).
#'
#' @author 
#' André Achim (Matlab)
#' P.-O. Caron (R)
#' 
#' @references 
#' À ajouter.
#' 
#' @return A data set the same size as D.
#' @export
#'
#' @examples
#' set.seed(73)
#' mydata <- data.frame(Likert = sample(1:7, replace = TRUE, size = 20),
#'                      Bernouilli = sample(0:1, replace = TRUE, size = 20))
#' ord.var <- detect.ordinal(mydata)
#' D <- genr8(n = 20, R = diag(ncol(mydata)))
#' colnames(D) <- c("Likert", "Bernouilli") 
#' replace.ordinal(ord.var, D)
replace.ordinal <- function(ord.var, D){
sapply(X = names(ord.var), 
       FUN = .replace.ordinal, 
       D = D, 
       ord.var = ord.var)
}
.replace.ordinal <- function(x, D, ord.var) {
  apply(D[x], MARGIN = 1,
        FUN = function(z, x, ord.var) {
          as.numeric(names(ord.var[[x]])[min(which(z < ord.var[[x]]))])
        },
        x = x, ord.var = ord.var)
}