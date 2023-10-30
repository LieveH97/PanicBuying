##############################################
###                                        ###
###       incidence-quantity model         ###
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


### REPORTING OUTPUT FUNCTIONS ###
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
fixed_effects_output_clean <- function(fe, name, keep_list) {
  html_output <- stargazer(fe, dep.var.labels.include = F, column.labels = "Unconditional Purchase Quantity", type = "html", keep=keep_list,
                           add.lines = list(c("Household FE", "yes"), c("Year FE", "yes"), c("Week of year FE", "yes"),  c("Day of week FE", "yes"), c("Observations", length(FullData$unconditional_Total_volume_sales))),
                           initial.zero = F, align = T, out = paste(project_path,"/R_Files/Supply-side concerns/Tables/",name,".html", sep=""))
  return(html_output)
}

OLS_output_clean <- function(ols, name, keep_list) {
  html_output <- stargazer(ols, dep.var.labels.include = F, column.labels = "Unconditional Purchase Quantity", type = "html", keep=keep_list,
                           add.lines = list(c("Household FE", "no"), c("Year FE", "yes"), c("Week of year FE", "yes"),  c("Day of week FE", "yes"), c("Observations", length(FullData$unconditional_Total_volume_sales))),
                           initial.zero = F, align = T, out = paste(project_path,"/R_Files/Supply-side concerns/Tables/",name,".html", sep=""))
  return(html_output)
}









##################################
# GfK QUESTIONNAIRE: RESPONDENTS #
##################################

# run analysis on households who participated in the GfK questionnaire

# load prepared dataset
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))
# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0
# load households
load(file=paste(project_path,"/R_files/Supply-side concerns/GfK-respondents",sep=""))
# save households who did not fill in survey
non.respondents <- unique(FullData$Household)[!unique(FullData$Household) %in% GfKrespondents]
save(non.respondents, file = paste(project_path,"/R_files/INC-QUANT model/toilet paper/non-respondents", sep=""))
# filter dataset
FullData <- FullData[FullData$Household %in% GfKrespondents,]

### TOBIT 2 - HECKMAN SELECTION MODEL ###
# main effects only, including controls
heckman_main <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_IPT 
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_Q   
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          data = FullData, method = "2step")
# clean version: excluding time fixed effects from output
keep <- c("PromoIndex","PanicIndex","HouseholdInventory","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")
# save clean output
two_stages_output_clean(heckman_main, "Heckman-2step-survey-respondents-robustness-check", keep)

rm(heckman_main)







#############################################################################
# GfK QUESTIONNAIRE: EXCLUDE HOUSEHOLDS WHO EXPERIENCED MORE OOS THAN USUAL #
#############################################################################

# Q11A-7 I have to visit multiple stores because the products I need are sold out at the store I usually visit
# run analysis on households who did not answer with "more often"

# load prepared dataset
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))
# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0
# load households
load(file=paste(project_path,"/R_files/Supply-side concerns/Q11A-7-exclude-more-often",sep=""))
load(file = paste(project_path,"/R_files/INC-QUANT model/toilet paper/non-respondents", sep=""))
# filter dataset
FullData <- FullData[FullData$Household %in% `Q11A-7-exclude-more-often` | FullData$Household %in% non.respondents,]

### TOBIT 2 - HECKMAN SELECTION MODEL ###
# main effects only, including controls
heckman_main <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_IPT 
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_Q   
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          data = FullData, method = "2step")
# clean version: excluding time fixed effects from output
keep <- c("PromoIndex","PanicIndex","HouseholdInventory","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")
# save clean output
two_stages_output_clean(heckman_main, "Heckman-2step-Q11A-7-robustness-check-with-non-responders", keep)

rm(heckman_main)








##################################################################################
# GfK QUESTIONNAIRE: EXCLUDE HOUSEHOLDS WHO BOUGHT TOILET PAPER LESS THAN BEFORE #
##################################################################################

#Q16-3 Now think about the categories you bought less often than before. Was this because they were not as readily available in the store as they were prior to the restrictions taking effect?
# run analysis on households with NA (= households who do not buy toilet paper less often than before = Q15-3)


# load prepared dataset
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))
# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0
# load households
load(file=paste(project_path, "/R_files/Supply-side concerns/Q16-3-only-NAs", sep=""))
load(file = paste(project_path,"/R_files/INC-QUANT model/toilet paper/non-respondents", sep=""))
# filter dataset
FullData <- FullData[FullData$Household %in% `Q16-3-only-NAs` | FullData$Household %in% non.respondents,]

### TOBIT 2 - HECKMAN SELECTION MODEL ###
# main effects only, including controls
heckman_main <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_IPT 
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_Q   
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          data = FullData, method = "2step")
# clean version: excluding time fixed effects from output
keep <- c("PromoIndex","PanicIndex","HouseholdInventory","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")
# save clean output
two_stages_output_clean(heckman_main, "Heckman-2step-Q16-3-NA-robustness-check-with-non-respondents", keep)

rm(heckman_main)








###########################################################################################
# GfK QUESTIONNAIRE: EXCLUDE HOUSEHOLDS WHO FOUND TOILET PAPER LESS AVAILABLE THAN BEFORE #
###########################################################################################

# Q16-3 Now think about the categories you bought less often than before. Was this because they were not as readily available in the store as they were prior to the restrictions taking effect?`
# run analysis on households who do not answer with "yes"
# load prepared dataset
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))
# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0
# load households
load(file=paste(project_path, "/R_files/Supply-side concerns/Q16-3-exclude-less-available", sep=""))
load(file = paste(project_path,"/R_files/INC-QUANT model/toilet paper/non-respondents", sep=""))
# filter dataset
FullData <- FullData[FullData$Household %in% `Q16-3-exclude-less-available` | FullData$Household %in% non.respondents,]

### TOBIT 2 - HECKMAN SELECTION MODEL ###
# main effects only, including controls
heckman_main <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_IPT 
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_Q   
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          data = FullData, method = "2step")
# clean version: excluding time fixed effects from output
keep <- c("PromoIndex","PanicIndex","HouseholdInventory","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")
# save clean output
two_stages_output_clean(heckman_main, "Heckman-2step-Q16-3-less-available-robustness-check-with-non-respondents", keep)

rm(heckman_main)










#######################
# MULTISTORE SHOPPERS #
#######################

# run analysis on households who buy at multiple stores (max 80% of trips at one chain)

# load prepared dataset
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))
# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0
# load households
load(file=paste(project_path, "/R_files/Supply-side concerns/Multistore_shoppers", sep=""))
# filter dataset
FullData <- FullData[FullData$Household %in% MultiStoreShopper,]

### TOBIT 2 - HECKMAN SELECTION MODEL ###
# main effects only, including controls
heckman_main <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_IPT 
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_Q   
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          data = FullData, method = "2step")
# clean version: excluding time fixed effects from output
keep <- c("PromoIndex","PanicIndex","HouseholdInventory","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")
# save clean output
two_stages_output_clean(heckman_main, "Heckman-2step-MultistoreShoppers-robustness-check", keep)

rm(heckman_main)















