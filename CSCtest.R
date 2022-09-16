library(dplyr)
library(tidyr)
library(janitor)
library(fPortfolio)
library(PerformanceAnalytics)
library(ggplot2)
library(ggrepel)
library(rmarkdown)
library(kableExtra)
library(BLCOP)

rm(list = ls())

load("IndexSet_CSC.Rda") ### From PullIdxFromBBG.R

lambda_mult <- 1
frequency <- "WEEKLY"
years <- 5
curncy <- "EUR"

annFactor <- switch(frequency,
                    "WEEKLY"= 52,
                    "DAILY" = 252,
                    "MONTHLY" = 12)

idxMap <- read.csv("CSC_SAA.csv", stringsAsFactors = F) %>%
  mutate(Idx = paste(Idx, CCY, sep = "_")) %>%
  select(-CCY)

IndexSetW <- IdxRets %>%
  left_join(idxMap, by = "Idx") %>%
  select(-c(Idx, TotW)) %>%
  pivot_wider(names_from = Name, values_from = Ret) %>% 
  select(-Date)

assetsNames <- names(IndexSetW)

source("BL_job.R")
