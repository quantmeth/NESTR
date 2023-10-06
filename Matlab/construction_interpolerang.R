fractions = c(.05, .5, .95)

interpoleRang <- function(data, fractions = c(.05, .5, .95)){
  cc <- .5
  
  data <- apply(data, 2, sort) # Regarless of subject
  f <-  (fractions * nrow(data) + cc)
  b <- data[floor(ff),]
  nb <- multiplicite(data, floor(ff))
  h <- data[ceiling(ff),]
  nh <- multiplicite(data, floor(ff))
  dat <- ort(data);
  ff <- fractions / nrow(data) + cc
  b <- ]
}

multiplicite(dat,i) {

nv <- dim(data)[2]
k <- length(i)