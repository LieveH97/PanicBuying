##################################################
###                                            ###
###         Detecting Purchase Limits          ###
###                                            ###
##################################################

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
focus.dates <- seq(from = as.Date("2020-01-01"), to = as.Date("2020-04-30"), by = "days")




##############################################################
# MAXIMUM AND MINIMUM NUMBER OF PACKAGES BOUGHT PER RETAILER #
##############################################################

load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/prepared_purchasedata_NL_final",sep=""))

### PREPARE DATA ###
# aggregate to Retailer-Date
maxminRolls_retailer_day <- TP_purch %>% group_by(Banner_name, Date_of_purchase) %>% summarize(MaximumRolls = max(Total_volume_sales),  MinimumRolls = min(Total_volume_sales))
maxminPacks_retailer_day <- TP_purch %>% group_by(Banner_name, Date_of_purchase) %>% summarize(MaximumPacks = max(Total_unit_sales), MinimumPacks = min(Total_unit_sales))

# exclude RestRetailer
maxminRolls_retailer_day <- maxminRolls_retailer_day[maxminRolls_retailer_day$Banner_name != "RestRetailer", ]
maxminPacks_retailer_day <- maxminPacks_retailer_day[maxminPacks_retailer_day$Banner_name != "RestRetailer", ]
# exclude 2018 and 2019 to make plots more clear
maxminRolls_retailer_day <- maxminRolls_retailer_day[maxminRolls_retailer_day$Date_of_purchase %in% focus.dates, ]
maxminPacks_retailer_day <- maxminPacks_retailer_day[maxminPacks_retailer_day$Date_of_purchase %in% focus.dates, ]

# turn into long format
maxminRolls_retailer_day <- gather(maxminRolls_retailer_day, key = condition, value, MaximumRolls:MinimumRolls)
maxminPacks_retailer_day <- gather(maxminPacks_retailer_day, key = condition, value, MaximumPacks:MinimumPacks)



### PLOTS ###
# max and min number of packs per retailer
ggplot(maxminPacks_retailer_day, aes(x = Date_of_purchase, y = value, colour = condition)) + geom_line() + facet_wrap(Banner_name~.) + 
  geom_vline(xintercept = as.Date("2020-03-01"), color = "blue") + ylim(0,12.5)
# max and min number of rolls per retailer
ggplot(maxminRolls_retailer_day, aes(x = Date_of_purchase, y = value, colour = condition)) + geom_line() + facet_wrap(Banner_name~.) + 
  geom_vline(xintercept = as.Date("2020-03-01"), color = "blue") + ylim(0,150)



### FOCUS ON DIRK AND PLUS ###

testDIRK <- TP_purch %>% group_by(Banner_name, Date_of_purchase) %>% summarize(numberPurchases = n())
testDIRK <- testDIRK[testDIRK$Banner_name == "DIRK",]
testDIRK <- merge(testDIRK, maxminPacks_retailer_day[maxminPacks_retailer_day$condition == "MaximumPacks",], by = c("Banner_name", "Date_of_purchase"))
testDIRK$maximumPacks <- testDIRK$value
testDIRK <- testDIRK[,-c(4,5)]

testPLUS <- TP_purch %>% group_by(Banner_name, Date_of_purchase) %>% summarize(numberPurchases = n())
testPLUS <- testPLUS[testPLUS$Banner_name == "Plus",]
testPLUS <- merge(testPLUS, maxminPacks_retailer_day[maxminPacks_retailer_day$condition == "MaximumPacks",], by = c("Banner_name", "Date_of_purchase"))
testPLUS$maximumPacks <- testPLUS$value
testPLUS <- testPLUS[,-c(4,5)]

rm(list = c('testDIRK', 'testPLUS'))
rm(list = c('maxminRolls_retailer_day', 'maxminPacks_retailer_day'))






### CHAIN-SPECIFIC REGRESSIONS ###
# aggregate to Retailer-Date
maxPacks_retailer_day <- TP_purch %>% group_by(Banner_name, Date_of_purchase) %>% summarize(MaximumPacks = max(Total_unit_sales))
# exclude RestRetailer
maxPacks_retailer_day <- maxPacks_retailer_day[maxPacks_retailer_day$Banner_name != "RestRetailer", ]



### 1/1/2020 - 28/2/2020    VS.    1/3/2020 - 30/4/2020
# during dates
during.dates <- seq(from = as.Date("2020-03-01"), to = as.Date("2020-04-30"), by = "day")
# before dates
before.dates <- seq(from = as.Date("2020-01-01"), to = as.Date("2020-02-28"), by = "day")
# only keep observations of these dates
maxPacks_retailer_day <- maxPacks_retailer_day[maxPacks_retailer_day$Date_of_purchase %in% before.dates | maxPacks_retailer_day$Date_of_purchase %in% during.dates,]
# flag 'during' observations
maxPacks_retailer_day$Panic <- ifelse(maxPacks_retailer_day$Date_of_purchase %in% during.dates, 1, 0)


### 1/3/2019 - 30/4/2019    VS.     1/3/2020 - 30-04-2020
# during dates
during.dates <- seq(from = as.Date("2020-03-01"), to = as.Date("2020-04-30"), by = "day")
# before dates
before.dates <- seq(from = as.Date("2019-03-01"), to = as.Date("2019-04-30"), by = "day")
# only keep observations of these dates
maxPacks_retailer_day <- maxPacks_retailer_day[maxPacks_retailer_day$Date_of_purchase %in% before.dates | maxPacks_retailer_day$Date_of_purchase %in% during.dates,]
# flag 'during' observations
maxPacks_retailer_day$Panic <- ifelse(maxPacks_retailer_day$Date_of_purchase %in% during.dates, 1, 0)


### 12/2/2020 - 11/3/2020    VS.    12/3/2020 - 11/4/2020
# during dates
during.dates <- seq(from = as.Date("2020-02-12"), to = as.Date("2020-03-11"), by = "day")
# before dates
before.dates <- seq(from = as.Date("2020-03-12"), to = as.Date("2020-04-11"), by = "day")
# only keep observations of these dates
maxPacks_retailer_day <- maxPacks_retailer_day[maxPacks_retailer_day$Date_of_purchase %in% before.dates | maxPacks_retailer_day$Date_of_purchase %in% during.dates,]
# flag 'during' observations
maxPacks_retailer_day$Panic <- ifelse(maxPacks_retailer_day$Date_of_purchase %in% during.dates, 1, 0)


### 12/3/2019 - 11/4/2019    VS.    12/3/2020 - 11/4/2020
# during dates
during.dates <- seq(from = as.Date("2019-03-12"), to = as.Date("2019-04-11"), by = "day")
# before dates
before.dates <- seq(from = as.Date("2020-03-12"), to = as.Date("2020-04-11"), by = "day")
# only keep observations of these dates
maxPacks_retailer_day <- maxPacks_retailer_day[maxPacks_retailer_day$Date_of_purchase %in% before.dates | maxPacks_retailer_day$Date_of_purchase %in% during.dates,]
# flag 'during' observations
maxPacks_retailer_day$Panic <- ifelse(maxPacks_retailer_day$Date_of_purchase %in% during.dates, 1, 0)




# ALL CHAINS
chains_test <- lm(MaximumPacks ~ Panic, maxPacks_retailer_day)
summary(chains_test)
# ALBERT HEIJN
AH_test <- lm(MaximumPacks ~ Panic, maxPacks_retailer_day[maxPacks_retailer_day$Banner_name == "Albert Heijn",])
summary(AH_test)
# ALDI
Aldi_test <- lm(MaximumPacks ~ Panic, maxPacks_retailer_day[maxPacks_retailer_day$Banner_name == "Aldi",])
summary(Aldi_test)
# DIRK
DIRK_test <- lm(MaximumPacks ~ Panic, maxPacks_retailer_day[maxPacks_retailer_day$Banner_name == "DIRK",])
summary(DIRK_test)
# JUMBO
Jumbo_test <- lm(MaximumPacks ~ Panic, maxPacks_retailer_day[maxPacks_retailer_day$Banner_name == "Jumbo",])
summary(Jumbo_test)
# LIDL
Lidl_test <- lm(MaximumPacks ~ Panic, maxPacks_retailer_day[maxPacks_retailer_day$Banner_name == "Lidl",])
summary(Lidl_test)
# PLUS
Plus_test <- lm(MaximumPacks ~ Panic, maxPacks_retailer_day[maxPacks_retailer_day$Banner_name == "Plus",])
summary(Plus_test)


rm(list = c('chains_test', 'AH_test', 'Aldi_test', 'DIRK_test', 'Jumbo_test', 'Lidl_test', 'Plus_test', 'maxPacks_retailer_day'))









############################
# PACKAGE SIZE PERSISTENCE #
############################

# package persistence metric (on week level)
TP_purch$Week <-strftime(TP_purch$Date_of_purchase, format = "%Y-W%V")
# aggregate data to week level
temp <- TP_purch %>% group_by(Panelist, Week) %>% summarize(n_bought = n(), sizes_bought = list(unique(Volume_per_unit)))
# re-order data
temp <- temp[order(temp$Panelist, temp$Week),]
# get values from previous purchase on the same row
temp <- temp %>% group_by(Panelist) %>% mutate(prev_sizes_bought = dplyr::lag(sizes_bought))
temp$prev_sizes_bought[temp$prev_sizes_bought == "NULL"] <- NA
# calculate choice persistence
temp <- temp %>% rowwise() %>% mutate(packagepersistence = (1/n_bought) * sum(sizes_bought %in% prev_sizes_bought) )
# calculate average across households
avg_package_persistence <- temp %>% group_by(Week) %>% summarise(AvgPackagePersistence = mean(packagepersistence) )
# average choice persistence of 2018 and 2019
mean(avg_package_persistence$AvgPackagePersistence[grepl("2018",avg_package_persistence$Week)|grepl("2019",avg_package_persistence$Week) ])
# min choice persistence of 2020
min(avg_package_persistence$AvgPackagePersistence[grepl("2020",avg_package_persistence$Week)])



# make plot
avg_package_persistence$Week_clean <- gsub("-W", "", avg_package_persistence$Week)
# only plot from 2019 onwards
package_persistence_plot <- avg_package_persistence[!grepl("2018",avg_package_persistence$Week),]
# make long instead of wide
package_persistence_plot <- package_persistence_plot %>% select(Week_clean, AvgPackagePersistence) %>% 
  gather(key = "variable", value = "value", -Week_clean)
package_persistence_plot$WeekNR <- rep(1:length(unique(package_persistence_plot$Week_clean)),1)
# plot
ggplot(package_persistence_plot, aes(x = WeekNR, y = value, group = variable)) + geom_line(aes(color = variable)) + 
  ylim(0,1) +  xlab("Week") + ylab("Average Package Persistence") + 
  scale_x_continuous(breaks = c(1,20,40,60,70), labels = c("2019W1","2019W20","2019W40","2020W08", "2020W18"))

rm(list = c('package_persistence_plot', 'avg_package_persistence', 'temp'))







####################
# UNIT PERSISTENCE #
####################

# package persistence metric (on week level)
TP_purch$Week <-strftime(TP_purch$Date_of_purchase, format = "%Y-W%V")
# aggregate data to week level
temp <- TP_purch %>% group_by(Panelist, Week) %>% summarize(n_bought = n(), units_bought = list(unique(Total_unit_sales)))
# re-order data
temp <- temp[order(temp$Panelist, temp$Week),]
# get values from previous purchase on the same row
temp <- temp %>% group_by(Panelist) %>% mutate(prev_units_bought = dplyr::lag(units_bought))
temp$prev_units_bought[temp$prev_units_bought == "NULL"] <- NA
# calculate choice persistence
temp <- temp %>% rowwise() %>% mutate(unitpersistence = (1/n_bought) * sum(units_bought %in% prev_units_bought) )
# calculate average across households
avg_unit_persistence <- temp %>% group_by(Week) %>% summarise(AvgUnitPersistence = mean(unitpersistence) )
# average choice persistence of 2018 and 2019
mean(avg_unit_persistence$AvgUnitPersistence[grepl("2018",avg_unit_persistence$Week)|grepl("2019",avg_unit_persistence$Week) ])
# min choice persistence of 2020
min(avg_unit_persistence$AvgUnitPersistence[grepl("2020",avg_unit_persistence$Week)])



# make plot
avg_unit_persistence$Week_clean <- gsub("-W", "", avg_unit_persistence$Week)
# only plot from 2019 onwards
unit_persistence_plot <- avg_unit_persistence[!grepl("2018",avg_unit_persistence$Week),]
# make long instead of wide
unit_persistence_plot <- unit_persistence_plot %>% select(Week_clean, AvgUnitPersistence) %>% 
  gather(key = "variable", value = "value", -Week_clean)
unit_persistence_plot$WeekNR <- rep(1:length(unique(unit_persistence_plot$Week_clean)),1)
# plot
ggplot(unit_persistence_plot, aes(x = WeekNR, y = value, group = variable)) + geom_line(aes(color = variable)) + 
  ylim(0,1) +  xlab("Week") + ylab("Average Package Persistence") + 
  scale_x_continuous(breaks = c(1,20,40,60,70), labels = c("2019W1","2019W20","2019W40","2020W08", "2020W18"))

rm(list = c('unit_persistence_plot', 'avg_unit_persistence', 'temp'))






















