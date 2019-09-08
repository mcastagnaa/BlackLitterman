source("BLfunct.R")

priorVarcov <- cov.mve(IndexSetW)$cov

### DEFView
pick <- newPMatrix(RegionSplit$Region, 1)
pick[1, 2] <- -1
pick[1, 4] <- 1

viewSD <- sqrt(pick %*% priorVarcov %*% t(pick)) * sqrt(annFactor)

confd <- 1/as.numeric(pick %*% priorVarcov %*% t(pick))

tau = 1/nrow(IndexSetW)

priorW <- RegionSplit$TotW
names(priorW) <- RegionSplit$Region

results <- data.frame(Ptfl = "Prior", 
                      lambdM = 1,
                      viewRetAnn = 0,
                      Mean = hMu %*% RegionSplit$TotW * annFactor,
                      Vol = sqrt(t(RegionSplit$TotW) %*% priorVarcov %*% RegionSplit$TotW) * sqrt(annFactor),
                      t(priorW),
                      stringsAsFactors = F)

rm(priorW)
for(lambdM in seq(from = 0.2, to = 1.6, by = 0.2)) {
  for(viewRetAnn in seq(from = 0.04, to = 0.16, by = 0.02)) {
  
    myView <- BLViews(pick, q = viewRetAnn/sqrt(annFactor), confidences = confd, RegionSplit$Region)
    
    writeLines(paste("Lambda:",lambdM,"View Return:", viewRetAnn))
    
    results <- rbind(results,
                     BLfunct(priorCov = priorVarcov, 
                             hMu = hMu,
                             RegionSplit = RegionSplit,
                             lambdaMult = lambdM,
                             tau = tau,
                             myView = myView) %>%
                       mutate(lambdM = lambdM,
                              viewRetAnn = viewRetAnn))
    
  }
}

chart_to <- results %>%
  #select(lambdM, viewRetAnn, Japan, EM, Ptfl) %>%
  #gather(Region, Weight, -c(lambdM, viewRetAnn, Ptfl)) %>%
  ggplot(aes(x = Japan, y = EM, size = viewRetAnn, shape = Ptfl, color = Ptfl)) +
  geom_point() +
  facet_wrap(~lambdM, ncol = 2)+
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_size_continuous(range = c(0.1, 3))

print(chart_to)
