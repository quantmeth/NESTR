# TEST ####
library(Rnest)
set.seed(42)
R <- ex_4factors_corr

# R <- ex_2factors ; 
ns = 200; ntime = 2
data <- replicate(n = ntime, MASS::mvrnorm(n = ns, mu = rep(0, ncol(R)), Sigma = R), simplify = FALSE)
data <- as.data.frame(do.call("rbind", data))
data$Time <- as.factor(rep(1:ntime, each = ns))
#data$i1 <- round(data$i1)
#data$i2 <- round(data$i2)
dim(data)

time.var = "Time"; nreps = 1000; nOrdinal = 10 ; max.factors <- Lederman(ncol(data)-!is.null(time.var)); gamma = 1-c(.01,.025,.05,.1,.50); alpha =.05
#max.factors = 3
# FUNCTION ####
nest2 <- function(data, 
                  time.var = NULL, 
                  nreps = 1000, 
                  nOrdinal = 10, 
                  max.factors = Lederman(ncol(data)-!is.null(time.var)),
                  gamma = 1 - c(.01,.025,.05,.1,.50),
                  alpha = .05){
  
  # ADD grouping variables (?)
  
  # Check if time.var is in the data set ####
  if(is.null(time.var)){
    data$time.var <- 1
    time.var <- "time.var"
  } else if(!any(colnames(data) %in% time.var)){
    data[time.var] <- time.var
  }
  
  # Detect and remove unique variable ####
  unique.v <- unique.var(data, time.var = time.var)
  data <- data[,!names(data) %in% unique.v$unique.variables]
  
  # Check for ordinal variables ####
  ord.var <- detect.ordinal(data[,!names(data) %in% time.var])
  
  # Compute correlation matrices ####
  R <- by(data[,!names(data) %in% time.var], 
          INDICES = data[,time.var], 
          FUN = cor)
  Rtotal <- cor(data[,!names(data) %in% time.var])
  # Commentaire :
  # L'utilise de Rtotal prend pour acquis que les sujets sont dans le même ordre,
  # peu importe le jeu de données. On ignore la variance intrasujet. 
  # Il ne faut donc pas de donner manquantes.
  # Rtotal est probablement incorrect, pour le reste, c'est négligeable, 
  # comme on se base sur R et non le jeu de données
  
  nv <- ncol(Rtotal)
  ntime <- dim(unique(data[time.var]))[1]
  ns <- nrow(data)/ntime
  
  
  res.eig <- data.frame(expand.grid(Position = 1:nv, Time = names(R)), 
                        Eigenvalues = unlist(lapply(R, 
                                                    FUN = function(x) eigen(x, 
                                                                            symmetric = TRUE, 
                                                                            only.values = TRUE)$values)))
  res.eig$Type <- "Empirical"
  #res.eig$p <- NA
  names(res.eig)[2] <- time.var
  row.names(res.eig) <- NULL
  
  # Check Bartlett test ####
  # If ns, then stop nest2 (TODO)
  BS <- BartlettSphericity(R = Rtotal, n = ns)
  if(BS$p > .05) {stop("Data contains only noise: chisq(", BS$df,") = ", round(BS$chisq,3),", p = ", round(BS$p,3))}
  # Commentaires : 
  # Probablement incorrect comme Rtotal est basée sur ns * time données et non ns.
  # Moyenne par temps utilisée pour n = ns/ntime = nrow(data)/dim(unique(data[time.var]))[1]
  
  EIG <- list()
  # eig <- eigen()
  
  # NEST begins ####
  for(k in 1:(max.factors)){
    
    if(k == 1){
      M <- list(model = lapply(R, function(x) {matrix(0, ncol(x), nrow(x))}))
    } else {
      M <- prep.model(R, k-1)
    }
    # check diag model == 1
    # then set to 1
    if(any(unlist(lapply(M$model, function(x) diag(x))) >= 1)){
      stop("Diagonal exceeds 1 at factors = ", k,"variables =", colnames(Rtotal)[which(diag(M$model) >= 1)])
    } else {
      M$model <- lapply(M$model, function(x) {diag(x) <- 1; x})
    }
    
    # The loop for each time
    eig <- lapply(M$model, 
                  FUN = function(Rmodel, nreps, ns, ord.var, k, res.eig){
                    # Loops begin
                    # eigloop <- matrix(0, ncol = 1, nrow = nreps) # ncol(Rmodel)
                    eigloop <- numeric()
                    {for(i in 1:nreps){
                      # Generate data ####
                      XX <- as.data.frame(MASS::mvrnorm(n = ns, 
                                                        mu = rep(0, ncol(Rmodel)),
                                                        Sigma = Rmodel))
                      
                      # Commentaires : 
                      # Probablement incorrect, comme on prend t (temps) matrices p x p, soit t*p*p cellules, 
                      # alors qu'on pourrait avoir une (t*p) x (t*p) matrices, soit (t*p)^2, ou t fois plus d'informations
                      
                      # Replace ordinal variable ####
                      
                      XX[names(ord.var)] <- sapply(X = names(ord.var), 
                                                   FUN = replace.ordinal, 
                                                   D = XX, 
                                                   ord.var = ord.var)
                      Rsample <- cor(XX)
                      eigloop[i] <- eigen(Rsample, symmetric = TRUE, only.values = TRUE)$values[k]};
                      c(res.eig[(res.eig$Time==parent.frame()$i[] & res.eig$Position==k),]$Eigenvalues, eigloop)}},
                  nreps = nreps, ns = ns, ord.var = ord.var, k = k, res.eig)
    names(eig) <- paste0(time.var,"==",names(eig))
    EIG[[paste0("Position==",k)]] <- eig
  }
  
  # pour CI
  ci.eig <- lapply(EIG, function(x, gamma) {sapply(x, quantile, probs = gamma)}, gamma)
  out <- expand.grid(Type = paste0((gamma)*100,"% CI"), Time = names(R), Position = 1:(max.factors))
  out$Eigenvalues <- unlist(ci.eig)
  names(out)[2] <- time.var
  ci.eig <- rbind(res.eig[colnames(out)], out)
  
  # pour p
  ci.eig$p <- NA
  pr <- simplify2array(lapply(EIG, function(x) {sapply(x, function(x) {sum(x >= x[1])})}))
  ci.eig[which(res.eig$Position %in% 1:max.factors),]$p <- c(t(pr)) / nreps
  #res.eig$Type <- NULL
  
  # pour nf
  f <- as.formula(paste0("Position ~ ", time.var))
  nfactors <- merge(aggregate(f, function(x) {min(x)-1},
                              data = na.omit(ci.eig[ci.eig$p>=alpha,])), ci.eig[ci.eig$Type =="Empirical",])
  names(nfactors)[2] <- "nfactors"
  nfactors[,c("Eigenvalues","p", "Type")] <- NULL
  
  # TODO TRANSFORMER LE OUTPUT EN DÉCISION
  return(list(nfactors = nfactors,
              eigenvalues = ci.eig))
  # add a summary (pour estimator, max.factors, nreps, alpha, item used, var, ord, unique) 
  # method = list(estimator = ,
  # grouping variables; "time.var"
  # alpha = ,
  # gamma = ,
  # max.factors = ,
  # nreps = 
  # items
  # ord.var =
  # unique = )
  #
  # PRINT ####
  #
  combined <- data.frame(Time = "Combined",
             Position = 1:max.factors,
             Eigenvalues = rowMeans(as.matrix(reshape(na.omit(ci.eig)[c(time.var,"Position","Eigenvalues")], idvar = "Position", timevar = time.var, direction = "wide")[-1])),
             p = combineProb(as.matrix(reshape(na.omit(ci.eig)[c(time.var,"Position","p")], idvar = "Position", timevar = time.var, direction = "wide")[-1])))
  colnames(combined)[1] <- time.var
  RES <- rbind(na.omit(ci.eig)[-1],
               combined)
  ###
} 

library(ggplot2)
col.pal <- c(grey(seq(0.75, .2, length.out = length(gamma))), "blue")
ggplot(ci.eig, aes(x = Position, y = Eigenvalues, color = Type)) + 
  geom_line(linetype = "dashed") +
  geom_point() +
  facet_wrap(~Time) + 
  scale_color_manual(values = col.pal) +
  scale_x_continuous(breaks = scales::pretty_breaks())+
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(title = paste("NEST")) +
  theme(legend.position = c(.8, .8))
