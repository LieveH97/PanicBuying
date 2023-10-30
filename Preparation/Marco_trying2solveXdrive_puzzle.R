##############################################
###                                        ###
###      Filtering households              ###
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


### set path to data files
if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
  data_path <- "X:/Retailing_Group_RawData/Hoarding data"
}else if (Sys.info()["nodename"] == "LAPTOP-KBL77S8J"){
  data_path <- ""
}else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
  data_path <- "X:/Retailing_Group_RawData/Hoarding data"
}


### set path to save intermediate files
if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
  save_path <- "X:/Retailing_Group/PhD Lieve/Project 1"
}else if (Sys.info()["nodename"] == "LAPTOP-KBL77S8J"){
  save_path <- ""
}else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
  save_path <- "X:/Retailing_Group/PhD Lieve/Project 1"
}
#load libraries - you have to install the packages before running the code!
list.of.packages <- c("plyr","data.table","readr")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)

# create country list
country_list <- c("NL","BE","DE","FR","UK")





#########################
# Loading household data
#########################
# set country
country <- country_list[country_list=="NL"]

#Household characteristics dataset
# Start the clock
ptm <- proc.time()
HH <- read.csv(paste(data_path,"/",country,"/panelist.csv",sep=""))
# Stop the clock
proc.time() - ptm


#try other command as comparison


# Data.table
# require("data.table")
# Start the clock
ptm <- proc.time()
HH2 <- fread(paste(data_path,"/",country,"/panelist.csv",sep=""))
# Stop the clock
proc.time() - ptm

# Readr
# require("readr")
# Start the clock
ptm <- proc.time()
HH3 <- read_csv(paste(data_path,"/",country,"/panelist.csv",sep=""))
# Stop the clock
proc.time() - ptm




# Compare speed and use also best before


#Try purchase data
# Start the clock
ptm <- proc.time()
purchase18 <- fread(paste(data_path,"/",country,"/purchase_promo_2018.csv",sep=""))
# Stop the clock
proc.time() - ptm


#Try purchase data
# Start the clock
ptm <- proc.time()
purchase19 <- fread(paste(data_path,"/",country,"/purchase_promo_2019.csv",sep=""))
# Stop the clock
proc.time() - ptm



#Try purchase data
# Start the clock
ptm <- proc.time()
purchase20 <- fread(paste(data_path,"/",country,"/purchase_promo_2020.csv",sep=""))
# Stop the clock
proc.time() - ptm