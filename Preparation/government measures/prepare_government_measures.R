##############################################
###                                        ###
###  Preparing government measures data    ###
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
list.of.packages <- c("plyr","tidyr","ggplot2", "tidyverse", "zoo", "readxl","readr")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)


# create country list
country_list <- c("NL","BE","GE","FR","UK")




##################################
# loading government measures data
##################################

temp_path <- paste(data_path,"/Government policy tracker/OxCGRT_timeseries_all.xlsx",sep="")
temp_all_dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"),by="days")
sheet_names <- excel_sheets(temp_path)
country_codes <- c("BEL", "DEU", "FRA", "GBR", "NLD")
BE_gov_tracker <- data.frame(Date=seq(as.Date("2020-01-01"),as.Date("2020-12-31"),by="days"))
DE_gov_tracker <- data.frame(Date=seq(as.Date("2020-01-01"),as.Date("2020-12-31"),by="days"))
NL_gov_tracker <- data.frame(Date=seq(as.Date("2020-01-01"),as.Date("2020-12-31"),by="days"))
UK_gov_tracker <- data.frame(Date=seq(as.Date("2020-01-01"),as.Date("2020-12-31"),by="days"))
FR_gov_tracker <- data.frame(Date=seq(as.Date("2020-01-01"),as.Date("2020-12-31"),by="days"))


# create list of dataframes
temp_list <- sapply(sheet_names, function(x) x=data.frame() )
# each dataframe is filled with data from one sheet
for (i in seq_along(sheet_names)) {
  temp_list[[i]] <- read_xlsx(temp_path, sheet=i)
  temp_list[[i]] <- temp_list[[i]][temp_list[[i]]$country_code %in% country_codes,]
  temp_list[[i]] <- temp_list[[i]][,-2]
  temp_list[[i]] <- data.frame(t(temp_list[[i]]))
  temp_list[[i]]$Date <- rownames(temp_list[[i]])
  colnames(temp_list[[i]]) <- c(country_codes,"Date")
  temp_list[[i]] <- temp_list[[i]][-1,]
  temp_list[[i]]$Date <- parse_date(temp_list[[i]]$Date, "%d%b%Y", locale = locale("en"))
  rownames(temp_list[[i]]) <- seq(nrow(temp_list[[i]]))
  temp_list[[i]] <- temp_list[[i]][temp_list[[i]]$Date %in% temp_all_dates,]
  
  BE_gov_tracker <- cbind(BE_gov_tracker, temp_list[[i]]$BEL)
  NL_gov_tracker <- cbind(NL_gov_tracker, temp_list[[i]]$NLD)
  UK_gov_tracker <- cbind(UK_gov_tracker, temp_list[[i]]$GBR)
  FR_gov_tracker <- cbind(FR_gov_tracker, temp_list[[i]]$FRA)
  DE_gov_tracker <- cbind(DE_gov_tracker, temp_list[[i]]$DEU)
  }
# rename list elements (these will become dataframe names)
names(temp_list) <- sheet_names
# unlist the list of dataframes
list2env(temp_list,.GlobalEnv)

colnames(BE_gov_tracker) <- c("Date", sheet_names)
colnames(NL_gov_tracker) <- c("Date", sheet_names)
colnames(UK_gov_tracker) <- c("Date", sheet_names)
colnames(FR_gov_tracker) <- c("Date", sheet_names)
colnames(DE_gov_tracker) <- c("Date", sheet_names)


rm(list=ls(pattern="^temp"))





######################################
# save each country's prepared dataset
######################################

save(BE_gov_tracker, file=paste(project_path,"/R_files/Preparation/government measures/BE_measures.RData",sep=""))
save(NL_gov_tracker, file=paste(project_path,"/R_files/Preparation/government measures/NL_measures.RData",sep=""))
save(DE_gov_tracker, file=paste(project_path,"/R_files/Preparation/government measures/DE_measures.RData",sep=""))
save(UK_gov_tracker, file=paste(project_path,"/R_files/Preparation/government measures/UK_measures.RData",sep=""))
save(FR_gov_tracker, file=paste(project_path,"/R_files/Preparation/government measures/FR_measures.RData",sep=""))














