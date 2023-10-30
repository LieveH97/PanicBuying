###################################################
###                                             ###
###       IDENTIFYING MULTISTORE SHOPPERS      ###
###                                            ###
###################################################

rm(list=ls())

### Relative path setting - adapting to the operating computer
getwd()
if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
  project_path <- "C:/Work at KU Leuven/Projects/RetailCOVID19"
}else if (Sys.info()["nodename"] == "LAPTOP-KBL77S8J"){
  project_path <- "C:/Users/lieve/Documents/GitHub/RetailCOVID19"
}else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
  project_path <- "C:/PhD KU Leuven/OneDrive - KU Leuven/Projects_Github/RetailCOVID19"
}
setwd(project_path)


location <- "local"

### set path to data files
if (location == "server") {
  if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
    data_path <- "X:/Retailing_Group_RawData/Hoarding data"
  }else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
    data_path <- "X:/Retailing_Group_RawData/Hoarding data" }
} else if (location == "local") { 
  if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
    data_path <- ""
  }else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
    data_path <- "C:/PhD KU Leuven/AiMark" } 
}

#load libraries - you have to install the packages before running the code!
list.of.packages <- c("plyr","tidyr","ggplot2", "tidyverse", "zoo", "readxl")

#Install package if they are not installed yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)

all.dates <- seq(from = as.Date("2018-01-01"), to = as.Date("2020-04-30"), by = "days")
estimation.dates <- seq(from = as.Date("2019-01-01"), to = as.Date("2020-04-30"), by = "days")
initialization.dates <- seq(from = as.Date("2018-01-01"), to = as.Date("2018-12-31"), by = "days")
country <- "NL"
load(paste(project_path,"/R_Files/INC-QUANT model/toilet paper/households_in_analysis", sep=""))




##############################
# LOADING 2018 PURCHASE DATA #
##############################

all.purch.2018 <- read.csv(paste(data_path, "/",country, "/purchase_promo_2018.csv", sep = ""))
# restrict to households of interest
all.purch.2018 <- all.purch.2018[all.purch.2018$Panelist %in% relevant_households,]



# get each household's purchase trips (= Household-Day-Chain combo)
all.purch.2018 <- all.purch.2018 %>% group_by(Panelist, Date_of_purchase, Banner_name) %>% summarize(totVal = sum(Total_value_sales))
# count number of trips per chain
all.purch.2018 <- all.purch.2018 %>% group_by(Panelist, Banner_name) %>% summarize(ChainVisits = n())
# get total number of trips across chains
all.purch.2018 <- all.purch.2018 %>% group_by(Panelist) %>% mutate(totVisits = sum(ChainVisits))
all.purch.2018 <- all.purch.2018 %>% group_by(Panelist, Banner_name) %>% mutate(Share = ChainVisits/totVisits)

# 126 single store shoppers at Jumbo (> 80% of shopping trips are at Jumbo)
Jumbo_singlestore <- all.purch.2018[all.purch.2018$Share > 0.8 & all.purch.2018$Banner_name == "Jumbo", ]
# 206 single store shoppers at AH
AH_singlestore <- all.purch.2018[all.purch.2018$Share > 0.8 & all.purch.2018$Banner_name == "Albert Heijn", ]


all.purch.2018$SingleStoreShopper <- all.purch.2018$ChainVisits >= 0.8*all.purch.2018$totVisits
SingleStoreShoppers <- all.purch.2018$Panelist[all.purch.2018$SingleStoreShopper == T]
MultiStoreShoppers <- relevant_households[!relevant_households %in% SingleStoreShoppers]

save(MultiStoreShopper, file=paste(project_path,"/R_files/Supply-side concerns/Multistore_shoppers",sep=""))

rm('all.purch.2018')


### DESCRIPTIVES OF MULTISTORE SHOPPING
all.purch.2018 <- read.csv(paste(data_path, "/",country, "/purchase_promo_2018.csv", sep = ""))
# restrict to households of interest
all.purch.2018 <- all.purch.2018[all.purch.2018$Panelist %in% relevant_households,]
# flag multistore shoppers
all.purch.2018$multistoreshopper <- all.purch.2018$Panelist %in% MultiStoreShoppers
#number of chains visited per day
all.purch.2018_day <- all.purch.2018 %>% group_by(Panelist, Date_of_purchase) %>% summarize(nChains_day = length(unique(Banner_name)))
mean(all.purch.2018_day$nChains_day)
mean(all.purch.2018_day$nChains_day[all.purch.2018_day$Panelist %in% MultiStoreShoppers])

# get week numbers
all.purch.2018$Week <- strftime(all.purch.2018$Date_of_purchase, format = "%V")
all.purch.2018_week <- all.purch.2018 %>% group_by(Panelist, Week) %>% summarize(nChains_week = length(unique(Banner_name)))
mean(all.purch.2018_week$nChains_week)
mean(all.purch.2018_week$nChains_week[all.purch.2018_week$Panelist %in% MultiStoreShoppers])



########################################
# MULTIPLE STORE SHOPPING TOILET PAPER #
########################################


# loading prepared toilet paper purchase data
load(paste(project_path,"/R_Files/INC-QUANT model/toilet paper/prepared_purchasedata_",country,sep=""))
# restrict to households of interest
TP_purch <- TP_purch[TP_purch$Panelist %in% relevant_households,]
TP_purch$Panelist <- as.character(TP_purch$Panelist)
# restrict to initialization period
TP_purch <- TP_purch[TP_purch$Date_of_purchase %in% initialization.dates,]

# get each household's purchase trips (= Household-Day-Chain combo)
TP_purch <- TP_purch %>% group_by(Panelist, Date_of_purchase, Banner_name) %>% summarize(totVal = sum(Total_value_sales))
# count number of trips per chain
TP_purch <- TP_purch %>% group_by(Panelist, Banner_name) %>% summarize(ChainVisits = n())
# get total number of trips across chains
TP_purch <- TP_purch %>% group_by(Panelist) %>% mutate(totVisits = sum(ChainVisits))
TP_purch$SingleStoreShopper <- TP_purch$ChainVisits >= 0.8*TP_purch$totVisits


TP_SingleStoreShoppers <- TP_purch$Panelist[TP_purch$SingleStoreShopper == T]
TP_MultiStoreShoppers <- unique(TP_purch$Panelist[!TP_purch$Panelist %in% TP_SingleStoreShoppers])

save(TP_MultiStoreShoppers, file=paste(project_path,"/R_files/Supply-side concerns/TP_Multistore_shoppers",sep=""))













# Save or (over-)spend? The impact of hard-discounter shopping on consumers' grocery outlay
# Els Gijsbrechts, Katia Campo, Mark Vroegrijk

#MSSh,t	Multiple-store shopping	Dummy equal to 1 when the household visits more than one chain in week t, 0 otherwise.



# Beyond promotion-based store switching: Antecedents and patterns of systematic multiple-store shopping.
# Gijsbrechts, E., Campo, K., & Nisol, P. (2008)

# single-store shopper if min. 80% of trips at one chain


















