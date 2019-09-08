library(dplyr)
library(tidyr)
library(janitor)
library(fPortfolio)
library(PerformanceAnalytics)
library(ggplot2)
library(ggrepel)
library(rmarkdown)
library(kableExtra)

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

source("paper_MKVZ.R")
source("paper_BL.R")
source("paper_BL_tradeoff.R")

render("paper1.Rmd")
