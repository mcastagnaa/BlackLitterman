library(dplyr)
library(tidyr)
library(fPortfolio)
library(ggplot2)
library(BLCOP)

rm(list = ls())
set.seed(1234)

load("IndexSet_CSC.Rda")

lambda_mult <- 1
annFactor <- 52

idxMap <- read.csv("CSC_SAA.csv", stringsAsFactors = F) %>%
  mutate(Idx = paste(Idx, CCY, sep = "_")) %>%
  select(-CCY)

IndexSetW <- IdxRets %>%
  left_join(idxMap, by = "Idx") %>%
  select(-c(Idx, TotW)) %>%
  pivot_wider(names_from = Name, values_from = Ret) %>% 
  select(-Date)

##############
hMu <- apply(IndexSetW, 2, mean)

### Alternative Cov matrix estimate
priorVarcov <- MASS::cov.mve(IndexSetW)$cov
idxMap <- idxMap[match(dimnames(priorVarcov)[[1]],idxMap$Name),] ### same order of the priorVarcov!

### lambda = 
lambda <-  as.numeric((idxMap$TotW %*% hMu)/(t(idxMap$TotW) %*% priorVarcov %*% idxMap$TotW))
lambda <- lambda * lambda_mult
#lambda = 1

### tau = 
tau = 1/nrow(IndexSetW)
tau = 1
#tau = 1/2

### Eq.Rets ########################################
eqMu <- (lambda * priorVarcov) %*% idxMap$TotW

### Eq Rets vs. Hist Rets
as.data.frame(hMu) %>%
  mutate(SubSAA = row.names(.),
         Set = "HistAnn") %>%
  rename(Returns = hMu) %>%
  rbind(as.data.frame(eqMu) %>%
          mutate(SubSAA = row.names(.),
                 Set = "Equilibrium") %>%
          rename(Returns = V1)) %>%
  mutate(Returns = Returns * annFactor) %>% ### not compounded
  ggplot(aes(x = SubSAA, y = Returns, fill = Set, 
             label = paste0(round(Returns * 100, 1),"%"))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(position=position_dodge(width=0.9), vjust=-0.25, size = 2.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Historical and Equilibrium annualized returns",
       subtitle = paste0("weekly data: ", min(IdxRets$Date),
                         " - ",  max(IdxRets$Date)),
       x = "", y = "")

### View 1: US mkt flat
pick1 <- newPMatrix(idxMap$Name, 1)
pick1[1, 3] <- 1
pick1RetAnn <- 0

myView <- BLViews(P = pick1, 
                  q = pick1RetAnn/sqrt(annFactor), 
                  confidences = 70,
                  assetNames = idxMap$Name)

print(myView)

### New returns from the View (Bayesian returns)
marketPosterior <- posteriorEst(views = myView, sigma = lambda*priorVarcov, mu = as.vector(eqMu), tau = tau)
print(marketPosterior)

data.frame(prior = marketPosterior@priorMean,
           posterior = marketPosterior@posteriorMean) %>%
  tibble::rownames_to_column(var = "SubSAA") %>%
  pivot_longer(-SubSAA, names_to = "Set", values_to = "Returns") %>%
  mutate(Returns = Returns * annFactor) %>%
  ggplot(aes(x = SubSAA, y = Returns, fill = Set, 
             label = paste0(round(Returns * 100, 1),"%"))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(position=position_dodge(width=0.9), vjust=-0.25, size = 2.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Equilibrium and Posterior annualized returns",
       x = "", y = "")
