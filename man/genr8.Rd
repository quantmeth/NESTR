% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genr8.R
\name{genr8}
\alias{genr8}
\title{Simplify the the generation from a Multivariate Normal Distributions}
\usage{
genr8(n = 1, mean = rep(0, max(1, ncol(R))), R = diag(1), ...)
}
\arguments{
\item{n}{the number of samples required.}

\item{mean}{an optinal vector giving the means of the variables. Default is 0.}

\item{R}{a positive-definite symmetric matrix specifying the covariance matrix of the variables.}

\item{...}{Argument of MASS::mvrnorm, such as \code{tol}, \code{empirical}, and \code{EISPACK}.}
}
\value{
A data frame of size n by ncol(R).
}
\description{
Speeds up the use of MASS::mvrnorm()
}
\examples{
set.seed(73)
mydata <- genr8(n = 10, R = ex_2factors, empirical = TRUE)
head(mydata)
cov(mydata)

}
\author{
P.-O. Caron
}
