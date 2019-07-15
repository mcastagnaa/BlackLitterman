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

### BASICS ##########################################################
varCov <- cov(IndexSetW)
VarVol <- sqrt(diag(varCov))*sqrt(annFactor)
AnnVol <- as.numeric(sqrt(t(RegionSplit$TotW) %*% varCov %*% RegionSplit$TotW) * sqrt(annFactor))
AnnRets <- Return.annualized(IndexSetW, scale = annFactor)

PortExpRetCapW <- AnnRets %*% RegionSplit$TotW

### MARKOWITZ B1927/NP1990/PAP1952 ##################################
mvSpec <- portfolioSpec()
constraints <- "LongOnly"

### Special portfolios ######
## equal weights
eqWPort <- RegionSplit[,c("Region","TotW")]
eqWPort$TotW <- 1/nrow(eqWPort)
setWeights(mvSpec) <- eqWPort$TotW

eqWPortfolio <- feasiblePortfolio(data = timeSeries(IndexSetW),
                                  spec = mvSpec)
resPtf <- data.frame(Ptfl = "eqW",
                     Mean = as.numeric(getTargetReturn(eqWPortfolio@portfolio)["mean"]) ,
                     Vol = as.numeric(getTargetRisk(eqWPortfolio@portfolio)["Cov"]),
                     t(getWeights(eqWPortfolio)))

# Cov is
PerVar <- as.numeric(sqrt(t(eqWPort$TotW) %*% varCov %*% eqWPort$TotW))
# mean is
PerMean <- apply(IndexSetW, 2, mean) %*% eqWPort$TotW

## minimum risk portfolio given returns = equal weights portfolio
minRskSpec <- portfolioSpec()
setTargetReturn(minRskSpec) <- apply(IndexSetW, 2, mean) %*% eqWPort$TotW
minRskPtfl <- efficientPortfolio(
  data = timeSeries(IndexSetW),
  spec = minRskSpec
)

resPtf <- rbind(resPtf,
                data.frame(Ptfl = "minRsk",
                           Mean = as.numeric(getTargetReturn(minRskPtfl@portfolio)["mean"]) ,
                           Vol = as.numeric(getTargetRisk(minRskPtfl@portfolio)["Cov"]),
                           t(getWeights(minRskPtfl)))
                )

## minimum variance portfolio
glbMinRiskSpec <- portfolioSpec()
glbMinRiskPtfl <- minvariancePortfolio(
  data = timeSeries(IndexSetW),
  spec = glbMinRiskSpec
)

print(glbMinRiskPtfl)

resPtf <- rbind(resPtf,
                data.frame(Ptfl = "glbMinRsk",
                           Mean = as.numeric(getTargetReturn(glbMinRiskPtfl@portfolio)["mean"]) ,
                           Vol = as.numeric(getTargetRisk(glbMinRiskPtfl@portfolio)["Cov"]),
                           t(getWeights(glbMinRiskPtfl)))
)

## tangency portfolio
tanSpec <- portfolioSpec()
setRiskFreeRate(tanSpec) <- 0
tanPtfl <- tangencyPortfolio(
  data = timeSeries(IndexSetW),
  spec = tanSpec
)

print(tanPtfl)

resPtf <- rbind(resPtf,
                data.frame(Ptfl = "tangency",
                           Mean = as.numeric(getTargetReturn(tanPtfl@portfolio)["mean"]) ,
                           Vol = as.numeric(getTargetRisk(tanPtfl@portfolio)["Cov"]),
                           t(getWeights(tanPtfl)))
)
resPtf$Ptfl <- as.character(resPtf$Ptfl)

## Results
ggplot(resPtf, aes(x = Vol, y =  Mean, label = Ptfl)) +
  geom_label() + 
  geom_point()

resPtf %>%
  select(1, 4:ncol(resPtf)) %>%
  gather(Region, Weight, -Ptfl) %>%
  ggplot(aes(x = Region, y = Weight, fill = Ptfl)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

resPtf %>%
  select(1, 4:ncol(resPtf)) %>%
  gather(Region, Weight, -Ptfl) %>%
  ggplot(aes(x = Ptfl, y = Weight, fill = Region)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

### Frontier #######
setNFrontierPoints(mvSpec) <- 20
longFrontier <- portfolioFrontier(
  data = timeSeries(IndexSetW),
  spec = mvSpec
)

tailoredFrontierPlot(longFrontier, frontier = c("upper"), return = "mu", risk = "Cov",
                     sharpeRatio = F)

#weightsPlot(longFrontier)
#text <- "Mean-Variance Portfolio - Long Only constraints"
#mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
#weightedReturnsPlot(longFrontier)
#covRiskBudgetsPlot(longFrontier)

## Constraints on individual asset
Cnsts <- c("minW[2] = 0.1")
constrFrontier <- portfolioFrontier(data = timeSeries(IndexSetW),
                                    spec = mvSpec,
                                    constraints = Cnsts)

tailoredFrontierPlot(constrFrontier, frontier = c("upper"), return = "mu", risk = "Cov",
                     sharpeRatio = F)

## Add group constraints
## Add box/group constraints


