library(BLCOP)
library(dplyr)
library(tidyr)
library(janitor)
library(PerformanceAnalytics)
library(fPortfolio)

rm(list = ls())

load("IndexSet.Rda")

annFactor <- switch(frequency,
                    "WEEKLY"= 52,
                    "DAILY" = 252,
                    "MONTHLY" = 12)

idxMap <- RegionSplit %>%
  mutate(Idx = gsub(" Index$", "", Ticker)) %>%
  select(-Ticker)

IndexSetW <- IndexSet %>%
  left_join(idxMap, by = "Idx") %>%
  select(-c(Idx, TotW))%>%
  spread(Region, Ret) %>% 
  `row.names<-`(.[,"Date"]) %>%
  select(-Date)

assetnames <- names(IndexSetW)
print(assetnames)

### MSCI synthetic ####################################
MXAC <- IndexSet %>%
  left_join(idxMap, by = "Idx") %>%
  group_by(Date) %>%
  summarise(MktRet = sum(Ret*TotW/100)) %>%
  as.data.frame() %>%
  `row.names<-`(.[,"Date"]) %>%
  select(-Date)

chart.CumReturns(MXAC, main = paste("Market returns", curncy, frequency), wealth.index = T)
chart.CumReturns(IndexSetW, main = paste("Regional returns", curncy, frequency), legend.loc = "topleft", wealth.index = T)

# MXAC <- MXAC %>%
#   select(-Date) %>%
#   as.matrix()

### BUILD #############################################
#priorMeans <- rep(0,6)
priorMeans <- apply(IndexSetW, 2, mean)

priorVarcov <- cov.mve(IndexSetW)$cov

volatilities <- sqrt(diag(priorVarcov))
print(volatilities*sqrt(annFactor))

pickMatrix <- matrix(c(0, -1, 0, +1, 0, 0), nrow = 1, ncol = 6)
#1/(pickMatrix %*% priorVarcov %*% t(pickMatrix))

views <- BLViews(P = pickMatrix, q = 0.05*sqrt(annFactor), 
                 confidences = as.numeric(1/(pickMatrix %*% priorVarcov %*% t(pickMatrix))), 
                 assetNames = assetnames)
print(views)


marketPosterior <- posteriorEst(views = views, sigma = priorVarcov, mu = as.vector(priorMeans), tau = 0.5)
print(marketPosterior)
optPorts <- optimalPortfolios.fPort(marketPosterior, optimizer = "minriskPortfolio")
print(optPorts)

### Alternative version ##############################
finViews <- matrix(ncol = length(assetnames), nrow = 1, dimnames = list(NULL, assetnames))
finViews[, 1:length(assetnames)] <- RegionSplit$TotW

views <- addBLViews(finViews, Return.cumulative(MXAC), 
                    1/(t(RegionSplit$TotW) %*% priorVarcov %*% RegionSplit$TotW), 
                    views)
print(views)

marketPosterior2 <- BLPosterior(as.matrix(IndexSetW), views, 
                                tau = 0.5, 
                                #kappa = 0.5,
                                marketIndex = as.matrix(MXAC), riskFree = rep(0, nrow(MXAC)))

optPorts2 <- optimalPortfolios.fPort(marketPosterior2, 
                                     constraints ="minW[1:length(assetnames)] = 0.03",
                                     optimizer = "minriskPortfolio")

print(optPorts2)



