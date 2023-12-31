% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reSym.R
\name{reSym}
\alias{reSym}
\title{Resymmetrized data.}
\usage{
reSym(x, alpha = 0.05)
}
\arguments{
\item{x}{A data frame.}

\item{alpha}{A confidence interval for the symterization.}
}
\value{
A list of class "resym" containing the "un"skew data, the variables that were "un"skewed, the parameter of "un"skewness, the equation used to un"skew"
}
\description{
TODO
}
\examples{
n <- 200
A <- rnorm(n)
B <- rlnorm(n, sd = 2)
C <- -rlnorm(n, sd = 2)
x <- cbind(A,B,C)
round(apply(x, 2, skew), 3)
reSym(x)
}
