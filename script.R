library(NlcOptim)

rm(list = ls())

obj <- function(wgt) {
  ptfRet <- sum(ret*wgt)
  retVar <- sqrt(t(wgt) %*% varCovar %*% wgt)
  sr  <- -(ptfRet-rf)/retVar
  return(sr)
}

con <- function(wgt) {
  f = NULL
  f = rbind(f, sum(wgt)-1)
  return(list(ceq = f, c = NULL))
}

### parameters
varCovar <- matrix(data = c(0.000576046,	0.000126261,	0.00012385,	0.000104201,	5.57911E-05,
                            0.000126261,	0.000411463,	9.88479E-05,	0.000100924,	0.000109183,
                            0.00012385,	9.88479E-05,	0.00038341,	6.42237E-05,	5.20799E-05,
                            0.000104201,	0.000100924,	6.42237E-05,	0.000291617,	4.6866E-05,
                            5.57911E-05,	0.000109183,	5.20799E-05,	4.6866E-05,	0.000155289), 
                   nrow = 5)

ret <- c(0.01, 0.05, 0.02, 0.035, 0.0136)

minWgt <- 0.0
maxWgt <- 0.3

rf <- 0.03

### initial guess
wgt <- c(1, 0.0, 0.0, 0.0, 0.0)

### optimization
result <- solnl(X = wgt, objfun = obj, confun = con, #A = sum(wgt), B = 1, #confun = conFun,
                lb = rep(minWgt, length(wgt)), ub = rep(maxWgt, length(wgt)))

solWgt <- result$par
solSR <- -result$fn

print(solWgt)
print(solSR)
