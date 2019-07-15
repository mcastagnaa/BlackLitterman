library(readxl)
library(NlcOptim)

rm(list = ls())

CorrMtx <- as.matrix(read_excel("CorrMtx_GS1999.xlsx", range = "b2:h8", col_names = F))
ExpRet <- as.matrix(read_excel("CorrMtx_GS1999.xlsx", range = "k2:k8", col_names = F))
VolVect <- as.matrix(read_excel("CorrMtx_GS1999.xlsx", range = "J2:J8", col_names = F))

CovarMtx <- VolVect %*% t(VolVect) * CorrMtx

obj <- function(wgt) {
  ptfRet <- sum(ExpRet*wgt)
  retVar <- sqrt(t(wgt) %*% CovarMtx %*% wgt)
  sr  <- -(ptfRet-rf)/retVar
  return(sr)
}

con <- function(wgt) {
  f = NULL
  f = rbind(f, sum(wgt)-1)
  return(list(ceq = f, c = NULL))
}

### parameters

minWgt <- 0.0
maxWgt <- 1.0
rf <- 0.00

### initial guess
wgt <- c(1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

### optimization
result <- solnl(X = wgt, objfun = obj, confun = con, 
                lb = rep(minWgt, length(wgt)), ub = rep(maxWgt, length(wgt)))

solWgt <- result$par
solSR <- -result$fn

print(round(solWgt, 4))
print(solSR)