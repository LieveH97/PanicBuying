##############################################
###                                        ###
###  Preparing toilet paper barcode file   ###
###                                        ###
##############################################

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
list.of.packages <- c("plyr","tidyr","ggplot2", "tidyverse", "zoo", "readxl")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)


# create country list
country_list <- c("NL","BE","GE","FR","UK")



#######################
# loading prepared data
#######################
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))

# final selection of relevant variables
variables.keep <- c("Total_volume_sales",                                         # DV
                    "PromoIndex", "PanicIndex",                                   # main variables under investigation
                    "PromoSensitivity", "PLShare","BrandLoyalty",                 # gain-seeking moderators
                    "HouseholdInventory", "Average_IPT", "Average_Q",             # loss-avoidance moderators
                    "PriceIndex", "Age", "HouseholdSize", "Income",               # controls
                    "Day_of_week", "Week", "Year",
                    "Household", "Date")                                          # needed for clustering of st. err.
FullData <- FullData[ , names(FullData) %in% variables.keep]
# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0


###################################
# FIXED EFFECTS MODEL (FINAL MODEL)
###################################

fe_mod <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                # gain-seeking moderators
                PromoIndex*PromoSensitivity + PanicIndex*PromoSensitivity + 
                PromoIndex*PLShare + PanicIndex*PLShare + 
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
# save output
fixed_effects_output(fe_mod_clus, "FinalModel_Study2_clustFE_demos_moderators")
# clean version: excluding time fixed effects from output
modelmatrix <- model.matrix(fe_mod)
keep_mod <- colnames(modelmatrix)
rm(modelmatrix)
keep_mod <- keep_mod[!grepl("Day",keep_mod) & !grepl("Week",keep_mod) & !grepl("Year",keep_mod)]



#####################################################
### RANDOM EFFECTS WITH CLUSTERED STANDARD ERRORS ###
#####################################################
re_mod <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                # gain-seeking moderators
                PromoIndex*PromoSensitivity + PanicIndex*PromoSensitivity + 
                PromoIndex*PLShare + PanicIndex*PLShare + 
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
              data = FullData, index= c("Household", "Date"), model = "random")
# variance-covariance matrix with clustered standard errors
vcov_RE_clus <- vcovHC(re_mod, type = "HC1", cluster = "group")
# add clustered standard errors to model output
re_mod_clus <- coeftest(re_mod, vcov = vcov_RE_clus)



phtest(fe_mod, re_mod)
























