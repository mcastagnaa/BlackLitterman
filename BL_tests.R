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
namesAnnSd <- sqrt(diag(priorVarcov)) * sqrt(annFactor)
namesCorr <- cor(IndexSetW)


lambda <-  as.numeric((RegionSplit$TotW %*% hMu)/(t(RegionSplit$TotW) %*% priorVarcov %*% RegionSplit$TotW))

### Eq.Rets ########################################
eqMu <- (lambda * priorVarcov) %*% RegionSplit$TotW

### Eq Rets vs. Hist Rets
as.data.frame(hMu) %>%
  mutate(Region = row.names(.),
         Set = "Historical") %>%
  rename(Returns = hMu) %>%
  rbind(as.data.frame(eqMu) %>%
          mutate(Region = row.names(.),
                 Set = "Equilibrium") %>%
          rename(Returns = V1)) %>%
  mutate(Returns = Returns * annFactor) %>%
  ggplot(aes(x = Region, y = Returns, fill = Set)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

### eq.Rets test
#testW <- inv(lambda * priorVarcov) %*% eqMu
#print(round(as.numeric(RegionSplit$TotW), 8) == round(as.numeric(testW), 8))

### Views #########################################
pick <- newPMatrix(RegionSplit$Region, 1)
pick[1, 2] <- -1
pick[1, 4] <- 1

viewSD <- sqrt(pick %*% priorVarcov %*% t(pick)) * sqrt(annFactor)

confd <- 1/as.numeric(pick %*% priorVarcov %*% t(pick))

tau = 1/nrow(IndexSetW)

myView <- BLViews(pick, q = 0.05/sqrt(annFactor), confidences = confd, RegionSplit$Region)

### New returns from the View (Bayesian returns)
marketPosterior <- posteriorEst(views = myView, sigma = priorVarcov, mu = as.vector(eqMu), tau = tau)
print(marketPosterior)

### Portfolio optimized for those new returns (using the MV optimization)
optPorts <- optimalPortfolios.fPort(marketPosterior, optimizer = "tangencyPortfolio")
print(optPorts)

BLoptData <- data.frame(Ptfl = "prior",
                        Mean = as.numeric(getTargetReturn(optPorts$priorOptimPortfolio@portfolio)["mu"]) * annFactor ,
                        Vol = as.numeric(getTargetRisk(optPorts$priorOptimPortfolio@portfolio)["Sigma"]) * sqrt(annFactor),
                        t(getWeights(optPorts$priorOptimPortfolio@portfolio))) %>%
  rbind(data.frame(Ptfl = "posterior",
                   Mean = as.numeric(getTargetReturn(optPorts$posteriorOptimPortfolio@portfolio)["mu"]) * annFactor ,
                   Vol = as.numeric(getTargetRisk(optPorts$posteriorOptimPortfolio@portfolio)["Sigma"]) * sqrt(annFactor),
                   t(getWeights(optPorts$posteriorOptimPortfolio@portfolio))))

print(BLoptData)

BLoptData %>%
  select(1, 4:ncol(BLoptData)) %>%
  gather(Region, Weight, -Ptfl) %>%
  ggplot(aes(x = Region, y = Weight, fill = Ptfl)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_y_continuous(labels= scales::percent)
