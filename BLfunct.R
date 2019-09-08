
BLfunct <- function(priorCov, 
                    hMu,
                    RegionSplit, 
                    lambdaMult = 1, 
                    tau, 
                    myView, 
                    optmzr = "tangencyPortfolio") {
  
  
  lambda <-  as.numeric((RegionSplit$TotW %*% hMu)/(t(RegionSplit$TotW) %*% priorVarcov %*% RegionSplit$TotW))
  lambda <- lambda * lambdaMult
  
  eqMu <- (lambda * priorVarcov) %*% RegionSplit$TotW
  
  marketPosterior <- posteriorEst(views = myView, sigma = priorCov, mu = as.vector(eqMu), tau = tau)
  
  optPorts <- optimalPortfolios.fPort(marketPosterior, optimizer = optmzr)
  
  BLoptData <- data.frame(Ptfl = "posterior",
                          Mean = as.numeric(getTargetReturn(optPorts$posteriorOptimPortfolio@portfolio)["mu"]) * annFactor ,
                          Vol = as.numeric(getTargetRisk(optPorts$posteriorOptimPortfolio@portfolio)["Sigma"]) * sqrt(annFactor),
                          t(getWeights(optPorts$posteriorOptimPortfolio@portfolio)))
  return(BLoptData)

}