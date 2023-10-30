#################################################################
###                                                           ###
###       ALTERNATIVE OPERATIONALIZATIONS MODERATORS          ###
###                                                           ###
#################################################################

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


all.dates <- seq(from = as.Date("2018-01-01"), to = as.Date("2020-04-30"), by = "days")
estimation.dates <- seq(from = as.Date("2019-01-01"), to = as.Date("2020-04-30"), by = "days")
initialization.dates <- seq(from = as.Date("2018-01-01"), to = as.Date("2018-12-31"), by = "days")
country <- "NL"
load(paste(project_path,"/R_Files/INC-QUANT model/toilet paper/households_in_analysis", sep=""))





########################################
# loading purchase data (all categories)
########################################

all.purch.2018 <- read.csv(paste(data_path, "/",country, "/purchase_promo_2018.csv", sep = ""))
# restrict to households of interest
all.purch.2018 <- all.purch.2018[all.purch.2018$Panelist %in% relevant_households,]
all.purch.2018$Promo <- ifelse(all.purch.2018$Promo == "yes", T, F)
# set up moderators dataframe
moderators <- all.purch.2018 %>% group_by(Panelist) %>% summarize(Total_budget = sum(Total_value_sales)/100)


# combine purchase data with barcode data to get PL flag and sub-brand
barcode <- read.csv(paste(data_path,"/",country,"/barcode.csv",sep=""))
all.purch.2018 <- merge(all.purch.2018, barcode, all.x =T, by = "Barcode")
rm(barcode)




#########################################
# CROSS-CATEGORY CONSUMER CHARACTERISTICS
#########################################


### PROMO SENSITIVITY ###
# total budget spent on Promo
Promo <- all.purch.2018 %>% group_by(Panelist) %>% summarise(CrossCat_Promo_value = sum(Total_value_sales[Promo==T])/100)
moderators <- merge(moderators, Promo, by="Panelist", all.x=T)
rm(Promo)
# Promo sensitivity = fraction of budget spent on Promo
moderators$CrossCat_Promo.sens_value <- moderators$CrossCat_Promo_value / moderators$Total_budget
moderators <- select(moderators,-"CrossCat_Promo_value")


### PRIVATE LABEL SHARE ###
all.purch.2018$PL <- ifelse(all.purch.2018$PL == "yes", T, F)
# total budget spent on PL
PL <- all.purch.2018 %>% group_by(Panelist) %>% summarise(CrossCat_PL_value = sum(Total_value_sales[PL==T], na.rm=T)/100)
moderators <- merge(moderators, PL, by="Panelist", all.x=T)
rm(PL)
# PL share = fraction of budget spent on PL
moderators$CrossCat_PL.share_value <- moderators$CrossCat_PL_value / moderators$Total_budget
moderators <- select(moderators,-"CrossCat_PL_value")




rm(all.purch.2018)




#######################################################################################
# CATEGORY-SPECIFIC CONSUMER CHARACTERISTICS: VALUE, VOLUME AND TRIP OPERATIONALIZATION
#######################################################################################

# continue with prepared toilet paper data = category specific 
load(paste(project_path,"/R_Files/INC-QUANT model/toilet paper/prepared_purchasedata_",country,sep=""))

# setting observation and initialization period
observation.dates <- seq(as.Date("2018-01-01"), as.Date("2020-05-01"), by="days")
initialization.dates <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by="days")

# get lists of households and subbrands
all.households <- unique(TP_purch$Panelist)
all.BSBretailers <- unique(TP_purch$BSBRetailer)

# restrict dataset to initialization period
TP_purch <- TP_purch[TP_purch$Date_of_purchase %in% initialization.dates,]


# total budget spent on toilet paper
SoB <- TP_purch %>% group_by(Panelist) %>% summarise(TP_budget = sum(Total_value_sales)/100)
moderators <- merge(moderators, SoB, by="Panelist", all.x=T)
rm(SoB)


### PROMO SENSITIVITY ###
# for trip operationalization: if multiple purchases on one day -> Promo_day > 1 if at least one purchase was Promo
TP_purch <- TP_purch %>% group_by(Panelist, Date_of_purchase) %>% mutate(Promo_day = sum(Promo))
# total budget and volume of promo toilet paper
Promo <- TP_purch %>% group_by(Panelist) %>% summarise(Promo_value = sum(Total_value_sales[Promo==1])/100, 
                                                       TP_volume = sum(Total_volume_sales), Promo_volume = sum(Total_volume_sales[Promo==1]),
                                                       TP_trips = length(unique(Date_of_purchase)), Promo_trips = length(unique(Date_of_purchase[Promo_day>=1])))
moderators <- merge(moderators, Promo, by="Panelist", all.x=T)
rm(Promo)
# Promo sensitivity = fraction of toilet paper budget spent on promo    &     fraction of toilet paper rolls bought in promo
moderators$Promo.sens_value <- moderators$Promo_value / moderators$TP_budget
moderators$Promo.sens_volume <- moderators$Promo_volume / moderators$TP_volume
moderators$Promo.sens_trip <- moderators$Promo_trips / moderators$TP_trips



### PRIVATE LABEL SHARE ###
TP_purch$PL <- TP_purch$PL.x
# # if multiple purchases on one day -> PL_day > 1 if at least one purchase was PL
TP_purch <- TP_purch %>% group_by(Panelist, Date_of_purchase) %>% mutate(PL_day = sum(PL))
# total budget and volume of PL toilet paper
PL <- TP_purch %>% group_by(Panelist) %>% summarise(PL_value = sum(Total_value_sales[PL==1])/100, 
                                                    PL_volume = sum(Total_volume_sales[PL==1]), 
                                                    PL_trips = length(unique(Date_of_purchase[PL_day>=1])))
moderators <- merge(moderators, PL, by="Panelist", all.x=T)
rm(PL)
# PL share = fraction of toilet paper budget spent on PL    &    fraction of toilet paper rolls bought as PL
moderators$PL.share_value <- moderators$PL_value / moderators$TP_budget
moderators$PL.share_volume <- moderators$PL_volume / moderators$TP_volume
moderators$PL.share_trip <- moderators$PL_trips / moderators$TP_trips






######################################
# mean-centering continuous moderators
######################################

# total budget
moderators$Total_budget_nonMC <- moderators$Total_budget
moderators$Total_budget <- moderators$Total_budget_nonMC - mean(moderators$Total_budget_nonMC)
# cross category characteristics
moderators$CrossCat_Promo.sens_value_nonMC <- moderators$CrossCat_Promo.sens_value
moderators$CrossCat_Promo.sens_value <- moderators$CrossCat_Promo.sens_value_nonMC - mean(moderators$CrossCat_Promo.sens_value_nonMC)
moderators$CrossCat_PL.share_value_nonMC <- moderators$CrossCat_PL.share_value
moderators$CrossCat_PL.share_value <- moderators$CrossCat_PL.share_value_nonMC - mean(moderators$CrossCat_PL.share_value_nonMC)

# category budget
moderators$TP_budget_nonMC <- moderators$TP_budget
moderators$TP_budget <- moderators$TP_budget_nonMC - mean(moderators$TP_budget_nonMC)
# category specific characteristics
moderators$Promo.sens_value_nonMC <- moderators$Promo.sens_value
moderators$Promo.sens_value <- moderators$Promo.sens_value_nonMC - mean(moderators$Promo.sens_value_nonMC)
moderators$Promo.sens_volume_nonMC <- moderators$Promo.sens_volume
moderators$Promo.sens_volume <- moderators$Promo.sens_volume_nonMC - mean(moderators$Promo.sens_volume_nonMC)
moderators$Promo.sens_trip_nonMC <- moderators$Promo.sens_trip
moderators$Promo.sens_trip <- moderators$Promo.sens_trip_nonMC - mean(moderators$Promo.sens_trip_nonMC)
moderators$PL.share_value_nonMC <- moderators$PL.share_value
moderators$PL.share_value <- moderators$PL.share_value_nonMC - mean(moderators$PL.share_value_nonMC)
moderators$PL.share_volume_nonMC <- moderators$PL.share_volume
moderators$PL.share_volume <- moderators$PL.share_volume_nonMC - mean(moderators$PL.share_volume_nonMC)
moderators$PL.share_trip_nonMC <- moderators$PL.share_trip
moderators$PL.share_trip <- moderators$PL.share_trip_nonMC - mean(moderators$PL.share_trip_nonMC)



save(moderators, file = paste(project_path, "/R_files/Robustness checks/alternative_moderators", sep=""))
rm(TP_purch)






###############################################
# INC-QUANT MODEL WITH ALTERNATIVE MODERATORS #
##############################################

# load prepared dataset
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))

# merge purchase data with new moderators
load(file = paste(project_path, "/R_files/Robustness checks/alternative_moderators", sep=""))
FullData <- merge(FullData, moderators, by.x = "Household", by.y = "Panelist", all.x = T) 



# final selection of relevant variables
variables.keep <- c("Total_volume_sales",                                         # DV
                    "PromoIndex", "PanicIndex",                                   # main variables under investigation
                    "PromoSensitivity", "Promo.sens_value", "Promo.sens_trip", "CrossCat_Promo.sens_value",
                    "PLShare", "PL.share_value", "PL.share_trip", "CrossCat_PL.share_value",
                    "BrandLoyalty",
                    "HouseholdInventory", "Average_IPT", "Average_Q",
                    "PriceIndex", "Age", "HouseholdSize", "Income",
                    "Day_of_week", "Week", "Year",
                    "Household", "Date") 
FullData <- FullData[ , names(FullData) %in% variables.keep]
# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0

# save clean output
fixed_effects_output_clean <- function(fe, name, keep_list) {
  html_output <- stargazer(fe, dep.var.labels.include = F, column.labels = "Unconditional Purchase Quantity", type = "html", keep=keep_list,
                           add.lines = list(c("Household FE", "yes"), c("Year FE", "yes"), c("Week of year FE", "yes"),  c("Day of week FE", "yes"), c("Observations", length(FullData$unconditional_Total_volume_sales))),
                           initial.zero = F, align = T, out = paste(project_path,"/R_Files/Robustness checks/",name,".html", sep=""))
  return(html_output)
}


### FIXED EFFECTS MODEL ###
fe_mod <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                # gain-seeking moderators
                PromoIndex*CrossCat_Promo.sens_value + PanicIndex*CrossCat_Promo.sens_value + 
                PromoIndex*CrossCat_PL.share_value + PanicIndex*CrossCat_PL.share_value + 
                PromoIndex*BrandLoyalty + PanicIndex*BrandLoyalty + 
                # loss avoidance moderators
                PromoIndex*Average_IPT + PanicIndex*Average_IPT + 
                PromoIndex*Average_Q + PanicIndex*Average_Q + 
                PromoIndex*HouseholdInventory + PanicIndex*HouseholdInventory +
                # demographics
                PromoIndex*Age + PanicIndex*Age + 
                PromoIndex*HouseholdSize + PanicIndex*HouseholdSize +
                PromoIndex*Income + PanicIndex*Income +
                # controls
                PriceIndex + Day_of_week + Week + Year,
              data = FullData, index= c("Household", "Date"), model = "within")
# variance-covariance matrix with clustered standard errors
vcov_FE_clus <- vcovHC(fe_mod, type = "HC1", cluster = "group")
# add clustered standard errors to model output
fe_mod_clus <- coeftest(fe_mod, vcov = vcov_FE_clus)
# clean version: excluding time fixed effects from output
modelmatrix <- model.matrix(fe_mod)
keep_mod <- colnames(modelmatrix)
rm(modelmatrix)
rm(fe_mod)
keep_mod <- keep_mod[!grepl("Day",keep_mod) & !grepl("Week",keep_mod) & !grepl("Year",keep_mod)]

fixed_effects_output_clean(fe_mod_clus, "Fixed-Effects-robustness_crosscategory", keep_mod)
rm(fe_mod_clus)





### FIXED EFFECTS MODEL ###
fe_mod <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                # gain-seeking moderators
                PromoIndex*Promo.sens_trip + PanicIndex*Promo.sens_trip + 
                PromoIndex*PL.share_trip + PanicIndex*PL.share_trip + 
                PromoIndex*BrandLoyalty + PanicIndex*BrandLoyalty + 
                # loss avoidance moderators
                PromoIndex*Average_IPT + PanicIndex*Average_IPT + 
                PromoIndex*Average_Q + PanicIndex*Average_Q + 
                PromoIndex*HouseholdInventory + PanicIndex*HouseholdInventory +
                # demographics
                PromoIndex*Age + PanicIndex*Age + 
                PromoIndex*HouseholdSize + PanicIndex*HouseholdSize +
                PromoIndex*Income + PanicIndex*Income +
                # controls
                PriceIndex + Day_of_week + Week + Year,
              data = FullData, index= c("Household", "Date"), model = "within")
# variance-covariance matrix with clustered standard errors
vcov_FE_clus <- vcovHC(fe_mod, type = "HC1", cluster = "group")
# add clustered standard errors to model output
fe_mod_clus <- coeftest(fe_mod, vcov = vcov_FE_clus)
# clean version: excluding time fixed effects from output
modelmatrix <- model.matrix(fe_mod)
keep_mod <- colnames(modelmatrix)
rm(modelmatrix)
rm(fe_mod)
keep_mod <- keep_mod[!grepl("Day",keep_mod) & !grepl("Week",keep_mod) & !grepl("Year",keep_mod)]


fixed_effects_output_clean(fe_mod_clus, "Fixed-Effects-robustness_tripspecification", keep_mod)
rm(fe_mod_clus)





### FIXED EFFECTS MODEL ###
fe_mod <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                # gain-seeking moderators
                PromoIndex*Promo.sens_value + PanicIndex*Promo.sens_value + 
                PromoIndex*PL.share_value + PanicIndex*PL.share_value + 
                PromoIndex*BrandLoyalty + PanicIndex*BrandLoyalty + 
                # loss avoidance moderators
                PromoIndex*Average_IPT + PanicIndex*Average_IPT + 
                PromoIndex*Average_Q + PanicIndex*Average_Q + 
                PromoIndex*HouseholdInventory + PanicIndex*HouseholdInventory +
                # demographics
                PromoIndex*Age + PanicIndex*Age + 
                PromoIndex*HouseholdSize + PanicIndex*HouseholdSize +
                PromoIndex*Income + PanicIndex*Income +
                # controls
                PriceIndex + Day_of_week + Week + Year,
              data = FullData, index= c("Household", "Date"), model = "within")
# variance-covariance matrix with clustered standard errors
vcov_FE_clus <- vcovHC(fe_mod, type = "HC1", cluster = "group")
# add clustered standard errors to model output
fe_mod_clus <- coeftest(fe_mod, vcov = vcov_FE_clus)
# clean version: excluding time fixed effects from output
modelmatrix <- model.matrix(fe_mod)
keep_mod <- colnames(modelmatrix)
rm(modelmatrix)
rm(fe_mod)
keep_mod <- keep_mod[!grepl("Day",keep_mod) & !grepl("Week",keep_mod) & !grepl("Year",keep_mod)]


fixed_effects_output_clean(fe_mod_clus, "Fixed-Effects-robustness_valuespecification", keep_mod)
