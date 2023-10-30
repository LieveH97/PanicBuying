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
                           add.lines = list(c("Year FE", "yes", "yes"), c("Week of year FE", "yes", "yes"),  c("Day of week FE", "yes", "yes")), initial.zero = F, aling = T,
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

variables.keep <- c("Total_volume_sales",                                         # DV
                    "PromoIndex", "PanicIndex",                                   # main variables under investigation
                    "PromoSensitivity", "PLShare","BrandLoyalty",                 # gain-seeking moderators
                    "HouseholdInventory", "Average_IPT", "Average_Q",             # loss-avoidance moderators
                    "PriceIndex", "Age", "HouseholdSize", "Income",               # controls
                    "Day_of_week", "Week", "Year",
                    "Household", "Date", "inversemills") 


# load prepared dataset
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))
# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0



### HECKMAN SELECTION MODEL MANUALLY ###
# main effects only, including controls
select_eq <- probit(!is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_IPT 
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                    x = TRUE, data = FullData)

inversemills <- invMillsRatio(select_eq)
FullData$inversemills <- inversemills$IMR1
# OR:
# inversemills <- dnorm(select_eq$linear.predictors)

out_eq <- lm(Total_volume_sales  ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_Q   
                          + PriceIndex + inversemills + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          data = FullData, subset = (!is.na(Total_volume_sales)))


stargazer(select_eq, out_eq, selection.equation = TRUE, dep.var.labels.include = F,        
          column.labels = c("Purchase incidence", "(Conditional) Purchase Quantity"), type="html", keep=variables.keep,
          add.lines = list(c("Year FE", "yes", "yes"), c("Week of year FE", "yes", "yes"),  c("Day of week FE", "yes", "yes")), initial.zero = F, aling = T,
          out = paste(project_path,"/R_Files/Supply-side concerns/Tables/Heckman_manual.html", sep=""))




### TWO-STEP ESTIMATION OF SELECTION PACKAGE ON FULL DATA ###

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
two_stages_output_clean <- function(heck, name, keep_list){
  heckman_both_stages <- heck
  heckman_both_stages$param$index$betaO <- heck$param$index$betaS
  heckman_both_stages$param$index$betaS <- heck$param$index$betaO
  html_output <- stargazer(heck, heckman_both_stages, selection.equation = TRUE, dep.var.labels.include = F,        
                           column.labels = c("Purchase incidence", "(Conditional) Purchase Quantity"), type="html", keep=keep_list,
                           add.lines = list(c("Year FE", "yes", "yes"), c("Week of year FE", "yes", "yes"),  c("Day of week FE", "yes", "yes")), initial.zero = F, aling = T,
                           out = paste(project_path,"/R_Files/Supply-side concerns/Tables/",name,".html", sep=""))
  return(html_output)
} 
two_stages_output_clean(heckman_main, "Heckman_twostep_fulldata", keep)



### TRYING MANUAL ESTIMATION FOR BAD FILTER ###

# loading the filters
load(paste(project_path,"/R_Files/Supply-side concerns/filters_6April23.RData", sep=""))

# Lidl CrossCat limit filter
FullData <- FullData[!FullData$Household %in% filters_6April23$HH_Lidl_crosscat_limit, ]

# main effects only, including controls
select_eq <- probit(!is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                    + HouseholdInventory + Average_IPT 
                    + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                    x = TRUE, data = FullData)

inversemills <- invMillsRatio(select_eq)
FullData$inversemills <- inversemills$IMR1
# OR:
# inversemills <- dnorm(select_eq$linear.predictors)

out_eq <- lm(Total_volume_sales  ~  PromoIndex + PanicIndex 
             + HouseholdInventory + Average_Q   
             + PriceIndex + inversemills + Age + HouseholdSize + Income + Day_of_week + Week + Year,
             data = FullData, subset = (!is.na(Total_volume_sales)))


stargazer(select_eq, out_eq, selection.equation = TRUE, dep.var.labels.include = F,        
          column.labels = c("Purchase incidence", "(Conditional) Purchase Quantity"), type="html", keep=variables.keep,
          add.lines = list(c("Year FE", "yes", "yes"), c("Week of year FE", "yes", "yes"),  c("Day of week FE", "yes", "yes")), initial.zero = F, aling = T,
          out = paste(project_path,"/R_Files/Supply-side concerns/Tables/Heckman_manual_Lidl_crosscatLimit.html", sep=""))




### TRYING TWO-STEP ESTIMATION OF SELECTION PACKAGE FOR BAD FILTER ###

# loading the filters
load(paste(project_path,"/R_Files/Supply-side concerns/filters_6April23.RData", sep=""))

# Lidl CrossCat limit filter
FullData <- FullData[!FullData$Household %in% filters_6April23$HH_Lidl_crosscat_limit, ]

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
two_stages_output_clean <- function(heck, name, keep_list){
  heckman_both_stages <- heck
  heckman_both_stages$param$index$betaO <- heck$param$index$betaS
  heckman_both_stages$param$index$betaS <- heck$param$index$betaO
  html_output <- stargazer(heck, heckman_both_stages, selection.equation = TRUE, dep.var.labels.include = F,        
                           column.labels = c("Purchase incidence", "(Conditional) Purchase Quantity"), type="html", keep=keep_list,
                           add.lines = list(c("Year FE", "yes", "yes"), c("Week of year FE", "yes", "yes"),  c("Day of week FE", "yes", "yes")), initial.zero = F, aling = T,
                           out = paste(project_path,"/R_Files/Supply-side concerns/Tables/",name,".html", sep=""))
  return(html_output)
} 
two_stages_output_clean(heckman_main, "Heckman_twostep_Lidl_crosscatLimit", keep)
