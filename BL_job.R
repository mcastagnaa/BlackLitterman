####
hMu <- apply(IndexSetW, 2, mean)

### Alternative Cov matrix estimate
set.seed(1234)
priorVarcov <- MASS::cov.mve(IndexSetW)$cov
namesAnnSd <- sqrt(diag(priorVarcov)) * sqrt(annFactor) ### not used anywhere
namesCorr <- cor(IndexSetW) ### not used anywhere

idxMap <- idxMap[match(dimnames(priorVarcov)[[1]],idxMap$Name),] ### same order of the priorVarcov!

### lambda = 
lambda <-  as.numeric((idxMap$TotW %*% hMu)/(t(idxMap$TotW) %*% priorVarcov %*% idxMap$TotW))
lambda <- lambda * lambda_mult

### tau = 
tau = 1/nrow(IndexSetW)
tau = 1

### Eq.Rets ########################################
eqMu <- (lambda * priorVarcov) %*% idxMap$TotW

### Eq Rets vs. Hist Rets
compRets <- as.data.frame(hMu) %>%
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

print(compRets)

### eq.Rets test
#testW <- inv(lambda * priorVarcov) %*% eqMu
#print(round(as.numeric(RegionSplit$TotW), 8) == round(as.numeric(testW), 8))

### View 1: US mkt flat
pick1 <- newPMatrix(idxMap$Name, 1)
pick1[1, 3] <- 1
pick1RetAnn <- 0

# writeLines(paste("View1 SD =", sqrt(pick1 %*% priorVarcov %*% t(pick1)) * sqrt(annFactor), 
#                  "View1 Ret Ann = ", pick1RetAnn))

#confd <- 1/as.numeric(pick1 %*% priorVarcov %*% t(pick1))

myView <- BLViews(P = pick1, 
                  q = pick1RetAnn/sqrt(annFactor), 
                  confidences = 70, #confd
                  assetNames = idxMap$Name)

print(myView)

### New returns from the View (Bayesian returns)
marketPosterior <- posteriorEst(views = myView, sigma = lambda*priorVarcov, mu = as.vector(eqMu), tau = tau)
print(marketPosterior)

RetsComp <- data.frame(prior = marketPosterior@priorMean,
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

print(RetsComp)

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

chrtBLoptData <- BLoptData %>%
  select(1, 4:ncol(BLoptData)) %>%
  gather(Region, Weight, -Ptfl) %>%
  ggplot(aes(x = Region, y = Weight, fill = Ptfl,
             label = paste0(round(Weight*100,1), "%"))) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position=position_dodge(width=0.9), vjust=-0.25, size = 2.5) +
  theme_bw() +
  scale_y_continuous(labels= scales::percent) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Pre/Post view portfolio weights",
       #subtitle = paste0("lambda: ", round(lambda,2)),
       x = "", y = "")
  

print(chrtBLoptData)
