###############################################################
###                                                         ###
###       JUMBO AND ALBERT HEIJN PURCHASE LIMIT             ###
###                                                         ###
###############################################################

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

### set path to save
if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
  save_path <- "X:/Retailing_Group/PhD Lieve/Project 1"
}else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
  save_path <- "X:/Retailing_Group/PhD Lieve/Project 1"
}

#load libraries - you have to install the packages before running the code!
list.of.packages <- c("plyr","tidyr","ggplot2", "tidyverse", "zoo", "readxl", "car", "sampleSelection", "stargazer", "haven","expss", "plm", "sandwich", "lmtest")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)


# create country list
country_list <- c("NL","BE","GE","FR","UK")


# set country
country <- country_list[country_list=="NL"]

two_stages_output_clean <- function(heck, name, keep_list){
  heckman_both_stages <- heck
  heckman_both_stages$param$index$betaO <- heck$param$index$betaS
  heckman_both_stages$param$index$betaS <- heck$param$index$betaO
  html_output <- stargazer(heck, heckman_both_stages, selection.equation = TRUE, dep.var.labels.include = F,        
                           column.labels = c("Purchase incidence", "(Conditional) Purchase Quantity"), type="html", keep=keep_list,
                           add.lines = list(c("Observations", heck$param$nObs, heck$param$N1), c("Year FE", "yes", "yes"), c("Week of year FE", "yes", "yes"),  c("Day of week FE", "yes", "yes")), initial.zero = F, aling = T,
                           out = paste(project_path,"/R_Files/Supply-side concerns/Tables/",name,".html", sep=""))
  return(html_output)
}

variables.keep <- c("Total_volume_sales",                                         # DV
                    "PromoIndex", "PanicIndex",                                   # main variables under investigation
                    "PromoSensitivity", "PLShare","BrandLoyalty",                 # gain-seeking moderators
                    "HouseholdInventory", "Average_IPT", "Average_Q",             # loss-avoidance moderators
                    "PriceIndex", "Age", "HouseholdSize", "Income",               # controls
                    "Day_of_week", "Week", "Year",
                    "Household", "Date") 




filter_twostagemodel <- function(filter, name) {
  load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))
  # data of households who passed the filter
  FullData_filtered <- FullData[!FullData$Household %in% filter, ]
  # data of households who are excluded based on filter
  FullData_excluded <- FullData[FullData$Household %in% filter, ]
  rm(FullData)
  # only keep relevant variables of Full Data
  FullData_filtered <- FullData_filtered[ , names(FullData_filtered) %in% variables.keep]
  FullData_excluded <- FullData_excluded[ , names(FullData_excluded) %in% variables.keep]
  # when no purchase that day -> make unconditional quantity zero
  FullData_filtered$unconditional_Total_volume_sales <- FullData_filtered$Total_volume_sales
  FullData_filtered$unconditional_Total_volume_sales[is.na(FullData_filtered$unconditional_Total_volume_sales)] <- 0
  FullData_excluded$unconditional_Total_volume_sales <- FullData_excluded$Total_volume_sales
  FullData_excluded$unconditional_Total_volume_sales[is.na(FullData_excluded$unconditional_Total_volume_sales)] <- 0
  
  # two-stage model on filtered data
  heckman_main <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                            + HouseholdInventory + Average_IPT 
                            + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                            outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                            + HouseholdInventory + Average_Q   
                            + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                            data = FullData_filtered)
  keep <- c("PromoIndex","PanicIndex","HouseholdInventory","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")
  two_stages_output_clean(heckman_main, paste("Model1_excl_",name), keep)
  rm(list = c('heckman_main', 'FullData_filtered'))
  
  # two-stage model on excluded households
  heckman_main <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                            + HouseholdInventory + Average_IPT 
                            + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                            outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                            + HouseholdInventory + Average_Q   
                            + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                            data = FullData_excluded)
  keep <- c("PromoIndex","PanicIndex","HouseholdInventory","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")
  two_stages_output_clean(heckman_main, paste("Model1_only_", name), keep)
  rm(list = c('heckman_main', 'FullData_excluded'))
}


filter_twostagemodel_2step <- function(filter, name) {
  load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))
  # data of households who passed the filter
  FullData <- FullData[!FullData$Household %in% filter, ]
  # only keep relevant variables of Full Data
  FullData <- FullData[ , names(FullData) %in% variables.keep]
  # when no purchase that day -> make unconditional quantity zero
  FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
  FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0

  # two-stage model on filtered data
  heckman_main <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                            + HouseholdInventory + Average_IPT 
                            + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                            outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                            + HouseholdInventory + Average_Q   
                            + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                            data = FullData, method = "2step")
  keep <- c("PromoIndex","PanicIndex","HouseholdInventory","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")
  two_stages_output_clean(heckman_main, paste("Model1_2step_",name), keep)
  rm(list = c('heckman_main', 'FullData'))

}



load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/prepared_purchasedata_NL_final",sep=""))
all.households <- unique(TP_purch$Panelist)
rm(TP_purch)






#############################################
# FILTERING ON PURCHASES AFTER 1 MARCH 2020 #
#############################################

# load 2020 purchase data
all.purch.2020 <- read.csv(paste(data_path, "/",country, "/purchase_promo_2020.csv", sep = ""))
# only keep purchases of relevant households
all.purch.2020 <- all.purch.2020[all.purch.2020$Panelist %in% all.households,]
# transform Date of Purchase variable
all.purch.2020$Date_of_purchase <- as.Date(all.purch.2020$Date_of_purchase)
# only keep purchases between 1 March 2020 and 30 April 2020
all.purch.2020 <- all.purch.2020[all.purch.2020$Date_of_purchase > as.Date("2020-02-28") & all.purch.2020$Date_of_purchase < as.Date("2020-05-01"),]
# get chains and RestRetailers right
AlbertHeijn_formats <- c("Albert Heijn", "AH XL", "AH to Go", "Albert.nl")
Jumbo_formats <- c("Jumbo", "Jumbo City", "Jumbo Foodmarkt")
all.purch.2020$Banner_name[all.purch.2020$Banner_name %in% AlbertHeijn_formats] <- AlbertHeijn_formats[1]
all.purch.2020$Banner_name[all.purch.2020$Banner_name %in% Jumbo_formats] <- Jumbo_formats[1]
chains_keep <- c("Albert Heijn", "Jumbo", "Plus", "Lidl", "Aldi", "DIRK")
all.purch.2020$Banner_name[!all.purch.2020$Banner_name %in% chains_keep] <- "RestRetailer"
# get value spent on groceries per household per retailer
all.purch.2020 <- all.purch.2020 %>% group_by(Panelist, Banner_name) %>% summarize(TotVal = sum(Total_value_sales))
# get total value spent on groceries per household, across chains
all.purch.2020 <- all.purch.2020 %>% group_by(Panelist) %>% mutate(TotGroceryBudget = sum(TotVal))
# get % of grocery budget spent at particular chain
all.purch.2020$MS_chain <- all.purch.2020$TotVal/all.purch.2020$TotGroceryBudget

# get all households that 
  # 1) buy anything at this chain during the limit period
  # 2) spend more than 20% of their grocery budget at this chain during the limit period
HH_Jumbo_crosscat_limit <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "Jumbo"])
HH_Jumbo_crosscat_limit_freq <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "Jumbo" & all.purch.2020$MS_chain > 0.2])
HH_AH_crosscat_limit <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "Albert Heijn"])
HH_AH_crosscat_limit_freq <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "Albert Heijn" & all.purch.2020$MS_chain > 0.2])
HH_Lidl_crosscat_limit <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "Lidl"])
HH_Lidl_crosscat_limit_freq <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "Lidl" & all.purch.2020$MS_chain > 0.2])
HH_Aldi_crosscat_limit <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "Aldi"])
HH_Aldi_crosscat_limit_freq <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "Aldi" & all.purch.2020$MS_chain > 0.2])
HH_Plus_crosscat_limit <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "Plus"])
HH_Plus_crosscat_limit_freq <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "Plus" & all.purch.2020$MS_chain > 0.2])
HH_DIRK_crosscat_limit <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "DIRK"])
HH_DIRK_crosscat_limit_freq <- unique(all.purch.2020$Panelist[all.purch.2020$Banner_name == "DIRK" & all.purch.2020$MS_chain > 0.2])

rm(all.purch.2020)





######################################
# FILTERING ON SINGLE STORE SHOPPERS #
######################################

# load all 2018 purchase data
all.purch.2018 <- read.csv(paste(data_path, "/",country, "/purchase_promo_2018.csv", sep = ""))
# only keep relevant households
all.purch.2018 <- all.purch.2018[all.purch.2018$Panelist %in% all.households,]
# get chains and RestRetailers right
AlbertHeijn_formats <- c("Albert Heijn", "AH XL", "AH to Go", "Albert.nl")
Jumbo_formats <- c("Jumbo", "Jumbo City", "Jumbo Foodmarkt")
all.purch.2018$Banner_name[all.purch.2018$Banner_name %in% AlbertHeijn_formats] <- AlbertHeijn_formats[1]
all.purch.2018$Banner_name[all.purch.2018$Banner_name %in% Jumbo_formats] <- Jumbo_formats[1]
chains_keep <- c("Albert Heijn", "Jumbo", "Plus", "Lidl", "Aldi", "DIRK")
all.purch.2018$Banner_name[!all.purch.2018$Banner_name %in% chains_keep] <- "RestRetailer"
# group purchases of same day at same retailer as one trip
all.purch.2018 <- all.purch.2018 %>% group_by(Panelist, Banner_name, Date_of_purchase) %>% summarize(value = sum(Total_value_sales))
# count number of trips per household per chain
all.purch.2018 <- all.purch.2018 %>% group_by(Panelist, Banner_name) %>% summarize(ChainTrips = n())
# get total number of trips per household
all.purch.2018 <- all.purch.2018 %>% group_by(Panelist) %>% mutate(TotalTrips = sum(ChainTrips))
# get share of trips to each chain
all.purch.2018$MS_chain <- all.purch.2018$ChainTrips / all.purch.2018$TotalTrips


# single-store shoppers = more than 80% of trips to that chain
HH_Jumbo_singlestore <- unique(all.purch.2018$Panelist[all.purch.2018$Banner_name == "Jumbo" & all.purch.2018$MS_chain > 0.8])
HH_AH_singlestore <- unique(all.purch.2018$Panelist[all.purch.2018$Banner_name == "Albert Heijn" & all.purch.2018$MS_chain > 0.8])
HH_Aldi_singlestore <- unique(all.purch.2018$Panelist[all.purch.2018$Banner_name == "Aldi" & all.purch.2018$MS_chain > 0.8])
HH_Lidl_singlestore <- unique(all.purch.2018$Panelist[all.purch.2018$Banner_name == "Lidl" & all.purch.2018$MS_chain > 0.8])
HH_Plus_singlestore <- unique(all.purch.2018$Panelist[all.purch.2018$Banner_name == "Plus" & all.purch.2018$MS_chain > 0.8])
HH_DIRK_singlestore <- unique(all.purch.2018$Panelist[all.purch.2018$Banner_name == "DIRK" & all.purch.2018$MS_chain > 0.8])

rm(all.purch.2018)






######################################
# ALBERT HEIJN ONLINE PURCHASE LIMIT #
######################################

# load 2020 purchase data
all.purch.2020 <- read.csv(paste(data_path, "/",country, "/purchase_promo_2020.csv", sep = ""))
# only keep purchases of relevant households
all.purch.2020 <- all.purch.2020[all.purch.2020$Panelist %in% all.households,]
# transform Date of Purchase variable
all.purch.2020$Date_of_purchase <- as.Date(all.purch.2020$Date_of_purchase)
# only keep purchases between 1 March 2020 and 30 April 2020
all.purch.2020 <- all.purch.2020[all.purch.2020$Date_of_purchase > as.Date("2020-02-28") & all.purch.2020$Date_of_purchase < as.Date("2020-05-01"),]
# get chains and RestRetailers right
AlbertHeijn_formats <- c("Albert Heijn", "AH XL", "AH to Go", "Albert.nl")
all.purch.2020$Banner_name[all.purch.2020$Banner_name %in% AlbertHeijn_formats] <- AlbertHeijn_formats[1]
# only keep online purchases at Albert Heijn
online <- c("online-bezorgen", "online-ophalen") 
all.purch.2020 <- all.purch.2020[all.purch.2020$Banner_name == "Albert Heijn" & all.purch.2020$Purchase_method %in% online,]

# get all households who purchased online at Albert Heijn during limit period
HH_AH_online_limit <- unique(all.purch.2020$Panelist)

rm(all.purch.2020)




# temporarily save these filters
filters_6April23 <- tibble::lst(HH_AH_crosscat_limit, HH_AH_crosscat_limit_freq, HH_AH_singlestore, HH_Jumbo_crosscat_limit, HH_Jumbo_crosscat_limit_freq, HH_Jumbo_singlestore,
                                HH_Aldi_crosscat_limit, HH_Aldi_crosscat_limit_freq, HH_Aldi_singlestore, HH_DIRK_crosscat_limit, HH_DIRK_crosscat_limit_freq, HH_DIRK_singlestore,
                                HH_Lidl_crosscat_limit, HH_Lidl_crosscat_limit_freq, HH_Lidl_singlestore, HH_Plus_crosscat_limit, HH_Plus_crosscat_limit_freq, HH_Plus_singlestore,
                                HH_AH_online_limit)
save(filters_6April23, file = paste(project_path,"/R_Files/Supply-side concerns/filters_6April23.RData", sep=""))

load(paste(project_path,"/R_Files/Supply-side concerns/filters_6April23.RData", sep=""))







################################################
# RERUN TWO-STAGE MODEL BASED ON FILTERED DATA #
################################################

# in manuscript: 2 step estimation instead of ML

### EVERYONE WHO PURCHASED AT JUMBO AND/OR ALBERT HEIJN ONLINE ###
Jumbo_AHonline_limit <- c(filters_6April23$HH_Jumbo_crosscat_limit,filters_6April23$HH_AH_online_limit)
filter_twostagemodel_2step(Jumbo_AHonline_limit, "Jumbo_AHonline_crosscat_limit")

### ALL JUMBO AND ALBERT HEIJN SINGLE STORE SHOPPERS ###
Jumbo_AH_singlestore <- c(filters_6April23$HH_AH_singlestore, filters_6April23$HH_Jumbo_singlestore)
filter_twostagemodel_2step(Jumbo_AH_singlestore, "Jumbo_AHsinglestore")






