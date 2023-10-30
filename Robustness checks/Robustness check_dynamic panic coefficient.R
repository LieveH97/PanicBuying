################################################################
###                                                          ###
###  Robustness check: need for dynamic Panic coefficient?   ###
###                                                          ###
################################################################

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






###################################################################################################################################################
#                                                       LIMITING THE PANIC PERIOD
###################################################################################################################################################


#######################
# loading prepared data
#######################
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))

# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0


### REPORTING OUTPUT FUNCTIONS ###
two_stages_output_clean <- function(heck, name, keep_list){
  heckman_both_stages <- heck
  heckman_both_stages$param$index$betaO <- heck$param$index$betaS
  heckman_both_stages$param$index$betaS <- heck$param$index$betaO
  html_output <- stargazer(heck, heckman_both_stages, selection.equation = TRUE, dep.var.labels.include = F,        
                           column.labels = c("Purchase incidence", "(Conditional) Purchase Quantity"), type="html", keep=keep_list,
                           add.lines = list(c("Observations", heck$param$nObs, heck$param$N1), c("Year FE", "yes", "yes"), c("Week of year FE", "yes", "yes"),  c("Day of week FE", "yes", "yes")), initial.zero = F, aling = T,
                           out = paste(project_path,"/R_Files/Robustness checks/",name,".html", sep=""))
  return(html_output)
} 

fixed_effects_output_clean <- function(fe, name, keep_list) {
  html_output <- stargazer(fe, dep.var.labels.include = F, column.labels = "Unconditional Purchase Quantity", type = "html", keep=keep_list,
                           add.lines = list(c("Household FE", "yes"), c("Year FE", "yes"), c("Week of year FE", "yes"),  c("Day of week FE", "yes"), c("Observations", length(FullData$unconditional_Total_volume_sales))),
                           initial.zero = F, align = T, out = paste(project_path,"/R_Files/Robustness checks/",name,".html", sep=""))
  return(html_output)
}





#######################
# limiting panic period
#######################

#temp.dates <- seq(from = as.Date("2019-01-01"), to = as.Date("2020-04-20"), by = "days")
#temp.dates <- seq(from = as.Date("2019-01-01"), to = as.Date("2020-04-10"), by = "days")
temp.dates <- seq(from = as.Date("2019-01-01"), to = as.Date("2020-03-31"), by = "days")

FullData <- FullData[FullData$Date %in% temp.dates,]



#################################
# RQ1: PURCHASE - INCIDENCE MODEL
#################################

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
keep <- c("PromoIndex","PanicIndex", "HouseholdInventory","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")
# save clean output
two_stages_output_clean(heckman_main, "Heckman-2step-limited-panic-31.03-robustness-check", keep)

rm(heckman_main)











##############################################
# RQ2: FIXED EFFECTS MODEL WITH ALL MODERATORS
##############################################

### FIXED EFFECTS MODEL WITH DEMOGRAPHICS AS MODERATORS

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
# clean version: excluding time fixed effects from output
modelmatrix <- model.matrix(fe_mod)
keep_mod <- colnames(modelmatrix)
rm(modelmatrix)
keep_mod <- keep_mod[!grepl("Day",keep_mod) & !grepl("Week",keep_mod) & !grepl("Year",keep_mod)]
# save clean output
fixed_effects_output_clean(fe_mod_clus, "Fixed-Effects-limited-panic-20.04-robustness-check", keep_mod)

















