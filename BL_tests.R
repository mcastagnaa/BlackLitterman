library(BLCOP)
library(dplyr)
library(tidyr)
library(janitor)
library(fPortfolio)
library(PerformanceAnalytics)
library(ggplot2)

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

assetsNames <- names(IndexSetW)
print(assetsNames)


####
hMu <- apply(IndexSetW, 2, mean)

### Alternative Cov matrix estimate
priorVarcov <- cov.mve(IndexSetW)$cov

lambda <-  as.numeric((RegionSplit$TotW %*% hMu)/(t(RegionSplit$TotW) %*% priorVarcov %*% RegionSplit$TotW))

### Eq.Rets ########################################
eqMu <- (lambda * priorVarcov) %*% RegionSplit$TotW

### eq.Rets test
#testW <- inv(lambda * priorVarcov) %*% eqMu
#print(round(as.numeric(RegionSplit$TotW), 8) == round(as.numeric(testW), 8))

### Views #########################################
pick <- newPMatrix(RegionSplit$Region, 1)
pick[1, 2] <- -1
pick[1, 4] <- 1

confd <- 1/as.numeric(pick %*% priorVarcov %*% t(pick))

tau = 1/nrow(IndexSetW)

myView <- BLViews(pick, q = 0.02/sqrt(annFactor), confidences = confd, RegionSplit$Region)

### New returns from the View (Bayesian returns)
marketPosterior <- posteriorEst(views = myView, sigma = priorVarcov, mu = as.vector(eqMu), tau = tau)
print(marketPosterior)

### Portfolio optimized for those new returns (using the MV optimization)
optPorts <- optimalPortfolios.fPort(marketPosterior, optimizer = "tangencyPortfolio")
print(optPorts)

