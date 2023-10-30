#######################################################
###                                                 ###
###       DESCRIPITVES & POSTERIOR ANALYSES         ###
###                                                 ###
#######################################################

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


#######################
# loading prepared data
#######################
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))

# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0

all.households <- as.character(unique(FullData$Household))
all.dates <- seq(from= as.Date("2019-01-01"), to = as.Date("2020-04-30"), by="days")





#############################################
#            SUMMARY STATISTICS             #
#############################################

# on mean-centered variables
summary_table_MC <- c(summary(FullData$PromoIndex), sd(FullData$PromoIndex))
summary_table_MC <- rbind(summary_table_MC,
                          c(summary(FullData$PanicIndex),sd(FullData$PanicIndex)), 
                          c(summary(FullData$PromoSensitivity), sd(FullData$PromoSensitivity)),
                          c(summary(FullData$PLShare), sd(FullData$PLShare)),
                          c(summary(FullData$BrandLoyalty), sd(FullData$BrandLoyalty)),
                          c(summary(FullData$Average_IPT), sd(FullData$Average_IPT)),
                          c(summary(FullData$Average_Q), sd(FullData$Average_Q)),
                          c(summary(FullData$HouseholdInventory), sd(FullData$HouseholdInventory)))
rownames(summary_table_MC) <- c("Promo Index", "Panic Index", "Promo Sensitivity", "PL Share", "Brand Loyalty", "Average IPT", "Average Q", "Inventory")
colnames(summary_table_MC) <- c("Min.","1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "St. Dev.")
summary_table_MC <- round(summary_table_MC, 2)

stargazer(summary_table_MC,column.labels = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "St. Dev."),
          out = paste(project_path,"/R_Files/Descriptives/Tables/SummaryStatistics_Meancentered.html", sep=""))


#on non-mean-centered variables
summary_table_nonMC <- c(summary(FullData$PromoIndex), sd(FullData$PromoIndex))
summary_table_nonMC <- rbind(summary_table_nonMC,
                          c(summary(FullData$PanicIndex),sd(FullData$PanicIndex)), 
                          c(summary(FullData$PromoSensitivity_nonMC), sd(FullData$PromoSensitivity_nonMC)),
                          c(summary(FullData$PLShare_nonMC), sd(FullData$PLShare_nonMC)),
                          c(summary(FullData$BrandLoyalty_nonMC), sd(FullData$BrandLoyalty_nonMC)),
                          c(summary(FullData$Average_IPT_nonMC), sd(FullData$Average_IPT_nonMC)),
                          c(summary(FullData$Average_Q_nonMC), sd(FullData$Average_Q_nonMC)),
                          c(summary(FullData$HouseholdInventory_nonMC), sd(FullData$HouseholdInventory_nonMC)))
rownames(summary_table_nonMC) <- c("Promo Index", "Panic Index", "Promo Sensitivity", "PL Share", "Brand Loyalty", "Average IPT", "Average Q", "Inventory")
colnames(summary_table_nonMC) <- c("Min.","1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "St. Dev.")
summary_table_nonMC <- round(summary_table_nonMC, 2)

stargazer(summary_table_nonMC,column.labels = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "St. Dev."),
          out = paste(project_path,"/R_Files/Descriptives/Tables/SummaryStatistics_nonMeancentered.html", sep=""))






#######################################
#            DESCRIPTIVES             #
#######################################

# use actual purchase data
Purchases <- FullData[!is.na(FullData$Total_volume_sales),]


### DESCRIPTIVES FOR PROMO ###
# only use 2019 data
dates2019 <- seq(from = as.Date("2019-01-01"), to = as.Date("2019-12-31"), by = "days")
Purchases2019 <- Purchases[Purchases$Date %in% dates2019, ]
FullData2019 <- FullData[FullData$Date %in% dates2019,]

# "NO PROMO" when PromoIndex = 0
# "PROMO" when PromoIndex > 0

# per household: get number of (non-)Promo days, number of purchases on (non-)Promo days
Promo_input <- FullData2019 %>% group_by(Household) %>% summarize(PromoDays = sum(ifelse(PromoIndex>0, 1, 0)),
                                                                   NoPromoDays = sum(ifelse(PromoIndex==0,1,0)),
                                                                   Purch = sum(!is.na(Total_volume_sales)),
                                                                   PromoDays_purch = sum(ifelse(PromoIndex>0 & !is.na(Total_volume_sales),1,0)),
                                                                   NoPromoDays_purch = sum(ifelse(PromoIndex==0 & !is.na(Total_volume_sales),1,0)))

Promo_input$percPromo <- Promo_input$PromoDays_purch / Promo_input$PromoDays
Promo_input$percNonPromo <- Promo_input$NoPromoDays_purch / Promo_input$NoPromoDays


### average Q bought
mean(Purchases2019$Total_volume_sales)
mean(Purchases2019$Total_volume_sales[Purchases2019$PromoIndex==0])
mean(Purchases2019$Total_volume_sales[Purchases2019$PromoIndex>0])


# create table for output
PromoOutput <- c(mean(Promo_input$percNonPromo), mean(Promo_input$percPromo, na.rm=T))
PromoOutput <- rbind(PromoOutput,
                c(mean(Purchases2019$Total_volume_sales[Purchases2019$PromoIndex==0]) , mean(Purchases2019$Total_volume_sales[Purchases2019$PromoIndex>0])))
rownames(PromoOutput) <- c( "Average Percentage of (Non-Promo or Promo) Days When Purchase Was Made", "Average Purchase Quantity")
colnames(PromoOutput) <- c("No Promo", "Promo")
stargazer(PromoOutput,out = paste(project_path,"/R_Files/Descriptives/Tables/Descriptives-Promo.html", sep=""))




### DESCRIPTIVES FOR PANIC ###
# from January 2020 onwards: only 'real', non-imputed data
dates2020 <- seq(from = as.Date("2020-01-01"), to = as.Date("2020-04-30"), by = "days")
Purchases2020 <- Purchases[Purchases$Date %in% dates2020, ]
FullData2020 <- FullData[FullData$Date %in% dates2020,]

# LOW PANIC when PanicIndex <= median
# HIGH PANIC when PanicIndex > median
medianPanic <- median(Purchases2020$PanicIndex)

# per household: get number of (non-)Promo days, number of purchases on (non-)Promo days
Panic_input <- FullData2020 %>% group_by(Household) %>% summarize(HighPanicDays = sum(ifelse(PanicIndex>medianPanic, 1, 0)),
                                                                  LowPanicDays = sum(ifelse(PanicIndex<=medianPanic, 1, 0)),
                                                                  Purch = sum(!is.na(Total_volume_sales)),
                                                                  HighPanicDays_purch = sum(ifelse(PanicIndex>medianPanic & !is.na(Total_volume_sales),1,0)),
                                                                  LowPanicDays_purch = sum(ifelse(PanicIndex<=medianPanic & !is.na(Total_volume_sales),1,0)))

Panic_input$percLowPanic <- Panic_input$HighPanicDays_purch / Panic_input$HighPanicDays
Panic_input$percHighPanic <- Panic_input$LowPanicDays_purch / Panic_input$LowPanicDays


### average Q bought
mean(Purchases2020$Total_volume_sales)
mean(Purchases2020$Total_volume_sales[Purchases2020$PanicIndex<=medianPanic])
mean(Purchases2020$Total_volume_sales[Purchases2020$PanicIndex>medianPanic])


# create table for output
PanicOutput <- c(mean(Panic_input$percLowPanic), mean(Panic_input$percHighPanic))
PanicOutput <- rbind(PanicOutput,
                c(mean(Purchases2020$Total_volume_sales[Purchases2020$PanicIndex<=medianPanic]) , mean(Purchases2020$Total_volume_sales[Purchases2020$PanicIndex>medianPanic])))
rownames(PanicOutput) <- c( "Average Percentage of (Low- or High-Panic) Days When Purchase Was Made", "Average Purchase Quantity")
colnames(PanicOutput) <- c("Low Panic", "High Panic")
stargazer(PanicOutput,out = paste(project_path,"/R_Files/Descriptives/Tables/Descriptives-Panic.html", sep=""))



########
# do not replicate finding of more households buying: percentage goes down between low and high panic
# tried with Panic == 0 and Panic > 0: then replicate finding, but only one day where Panic == 0
# tried with Panic <= median and Panic > median: replicate finding, and 62 high promo, 59 low promo
summary(FullData2020$PanicIndex[FullData2020$Household == "NL-1000467"])















