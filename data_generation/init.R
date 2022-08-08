source("sim_func.R")
source("miss_func.R")


library(parallel)
library(rlecuyer)
library(mvtnorm)
library(mice)
library(mitools)
library(dplyr)
library(magrittr)



## Parse command line arguments:
outDir  <- "~/folder Thesis MI pred/Output/"


## Define levels of variable simulation parameters:
r2  <- c(0.1, 0.3)             # R-Squared
impmethod <- c("cart","pmm", "rf")
missingtype <- c("MCAR", "MAR", "pMNAR")

conds <- expand.grid(r2 = r2, impmethod = impmethod, missingtype = missingtype, stringsAsFactors = FALSE)

## Define the fixed simulation parameters:
parms <- list()
parms$verbose    <- FALSE
parms$miceIters  <- 20
parms$outDir     <- outDir
parms$incompVars <- c("y", "x1", "x2", "x3", "x4")
parms$MARpred    <- c("x5", "x6", "x7", "x8")
parms$MNARpred   <- c("y", "x1", "x2", "x3", "x4")
parms$missType   <- c("high", "low", "center", "tails", "low")
parms$coefs      <- matrix(c(1.0, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33, 0.33))
parms$varNames   <- c("y", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
parms$model      <- as.formula("y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10")
parms$mySeed     <- 235711
parms$nStreams   <- 500
parms$nObs       <- 500
parms$nImps      <- 20
parms$covX       <- 0.1
parms$nVal       <- 50


cl <- makeCluster(4)
  
## Export stuff to the cluster workers:
clusterEvalQ(cl, c(library(rlecuyer), library(mvtnorm), library(mice), library(mitools), source("sim_func.R"), source("miss_func.R")))
  
## Run the simulation in parallel:
  
it <- seq(1, 500, 1)
  
system.time(
  parLapply(cl = cl, it, fun = doRep, parms = parms, conds = conds)
  )
  stopCluster(cl)
  