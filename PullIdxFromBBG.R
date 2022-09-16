library(Rblpapi)
library(dplyr)
library(openxlsx)

rm(list = ls())

blpConnect()

startDate <- as.Date("2017-07-31") 
endDate <- as.Date("2022-07-31")

Indx <- c("MXEF Index", "MXJP Index", "MXNA Index",
          "MXEUG Index", "MXAPJ Index", "MXGB Index")

totRetField <- "DAY_TO_DAY_TOT_RETURN_NET_DVDS"
CCY <- "EUR"

if(exists("IdxRets")) rm(IdxRets)
i = "MXEF Index"
for(i in Indx){
  print(i)
  
  set <- bdh(i, 
             start.date = startDate,
             end.date = endDate,
             fields = tidyselect::all_of(totRetField),
             options = c("periodicitySelection" =  "WEEKLY",
                         "currency" = CCY)) %>%
    rename(Rets = totRetField) %>%
    mutate(Rets = Rets/100,
           Idx = gsub(" Index", "", i),
           CCY = CCY) %>%
    rename(Date = date)
  
  if(exists("IdxRets")) IdxRets = rbind(IdxRets, set) else IdxRets = set
}

files <- dir(path = "./ICEidx",  pattern = ".csv", full.names = T)
file <- files[1]
for(file in files) {
  idx <- read.csv(file, header = F, stringsAsFactors = F)[1,2]
  ccy <- read.csv(file, header = F, stringsAsFactors = F)[1,4]
  ccy <- paste0(ccy, ifelse(grepl("H$", read.csv(file, header = F, stringsAsFactors = F)[2,2]), "h", ""))
  
  print(paste(idx, ccy))
  
  set <- read.csv(file, stringsAsFactors = F, skip = 1)[, 1:2] %>%
    rename_all(~c("Date", "Rets")) %>%
    mutate(Date = as.Date(Date, "%d/%m/%Y"),
           Rets = Rets/100,
           Idx = idx,
           CCY = ccy)
  
  if(exists("IdxRets")) IdxRets = rbind(IdxRets, set) else IdxRets = set
}
rm(set, idx, file, files, ccy, i, CCY, totRetField)

IdxRets <- IdxRets %>%
  pivot_wider(names_from = c(Idx, CCY), values_from = Rets) %>%
  dplyr::filter(complete.cases(.)) %>% ### Ideally you should solve for the Dates mismatches (25/12 and so on)
  pivot_longer(-Date, names_to = "Idx", values_to = "Ret")

save(IdxRets, file = "IndexSet_CSC.Rda")
