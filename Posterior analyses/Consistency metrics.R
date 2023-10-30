#######################################################
###                                                 ###
###         POSTERIOR: CONSISTENCY METRICS          ###
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

country <- "NL"

# need sub-brand of each purchase
load(paste(project_path,"/R_Files/INC-QUANT model/toilet paper/prepared_purchasedata_",country,sep=""))




#############################
#    choice persistence     #
#############################

### PERCENTAGE OF PEOPLE BUYING BRANDSUBBRANDRETAILER THEY NEVER BOUGHT BEFORE ###
# cut-off = first covid case in the Netherlands (February 27, 2020)
dates.after <- seq(from = as.Date("2020-02-27"), to = as.Date("2020-04-30"), by= "days")
# take out all spaces, so that each BSBRetailer is one word --> easier to check whether in a list
TP_purch$BSBRetailer_clean <- paste(TP_purch$Banner_name, TP_purch$BrandSubBrand, sep="")
TP_purch$BSBRetailer_clean <- gsub(" ", "", TP_purch$BSBRetailer_clean)
# create list of previously-bought BSBRetailer units
temp1 <- TP_purch %>% filter(!Date_of_purchase %in% dates.after) %>% group_by(Panelist) %>% summarise(set_2019 = list(unique(BSBRetailer_clean)))
TP_purch <- merge(TP_purch, temp1, by="Panelist", all.x=TRUE)
rm(temp1)
# check for each purchase whether bought before
TP_purch_2020 <- TP_purch %>% filter(Date_of_purchase %in% dates.after) %>% rowwise() %>% mutate(boughtbefore = BSBRetailer_clean %in% set_2019)
# if not all purchases by a household where bought before (at least one false), there's a forced switch
post_analysis <- TP_purch_2020 %>% group_by(Panelist) %>% summarize(forcedswitch = !all(boughtbefore))
HH_forcedswitch <- sum(post_analysis$forcedswitch) / length(post_analysis$forcedswitch)



### PERCENTAGE OF PEOPLE BUYING BRANDSUBBRAND THEY NEVER BOUGHT BEFORE ###
# take out retailer aspect, to account for store changes due to lockdown (e.g. WFH, so not store on the way to work)
# cut-off = first covid case in the Netherlands (February 27, 2020)
dates.after <- seq(from = as.Date("2020-02-27"), to = as.Date("2020-04-30"), by= "days")
TP_purch$BSB_clean <- TP_purch$BrandSubBrand
TP_purch$BSB_clean <- gsub(" ", "", TP_purch$BSB_clean)
temp1 <- TP_purch %>% filter(!Date_of_purchase %in% dates.after) %>% group_by(Panelist) %>% summarise(set_2019_BSB = list(unique(BSB_clean)))
TP_purch <- merge(TP_purch, temp1, by="Panelist", all.x=TRUE)
rm(temp1)
TP_purch_2020 <- TP_purch %>% filter(Date_of_purchase %in% dates.after) %>% rowwise() %>% mutate(boughtbefore = BSB_clean %in% set_2019_BSB)
post_analysis <- TP_purch_2020 %>% group_by(Panelist) %>% summarize(forcedswitch_noretailer = !all(boughtbefore))
HH_forcedswitch_noretailer <- sum(post_analysis$forcedswitch_noretailer) / length(post_analysis$forcedswitch_noretailer)

rm(list = c('TP_purch_2020', 'post_analysis'))






### CHOICE PERSISTENCE ###

# choice persistence metric (on week level)
TP_purch$Week <-strftime(TP_purch$Date_of_purchase, format = "%Y-W%V")
# aggregate data to week level
temp <- TP_purch %>% group_by(Panelist, Week) %>% summarize(n_bought = n(),BSB_bought = list(unique(BSB_clean)))
# re-order data
temp <- temp[order(temp$Panelist, temp$Week),]
# get values from previous purchase on the same row
temp <- temp %>% group_by(Panelist) %>% mutate(prev_BSB_bought = dplyr::lag(BSB_bought))
temp$prev_BSB_bought[temp$prev_BSB_bought == "NULL"] <- NA
# calculate choice persistence
temp <- temp %>% rowwise() %>% mutate(choicepersistence_BSB = (1/n_bought) * sum(BSB_bought %in% prev_BSB_bought) )
# calculate average across households
post_analysis_choicepers <- temp %>% group_by(Week) %>% summarise(AvgChoicePersistence_BSB = mean(choicepersistence_BSB) )
# average choice persistence of 2018 and 2019
mean(post_analysis_choicepers$AvgChoicePersistence_BSB[grepl("2018",post_analysis_choicepers$Week)|grepl("2019",post_analysis_choicepers$Week) ])
# min choice persistence of 2020
min(post_analysis_choicepers$AvgChoicePersistence_BSB[grepl("2020",post_analysis_choicepers$Week)])


# make plot
post_analysis_choicepers$Week_clean <- gsub("-W", "", post_analysis_choicepers$Week)
# only plot from 2019 onwards
post_analysis_choicepers_plot <- post_analysis_choicepers[!grepl("2018",post_analysis_choicepers$Week),]
post_analysis_choicepers_plot$WeekNR <- 1:length(unique(post_analysis_choicepers_plot$Week_clean))
# plot
ggplot(post_analysis_choicepers_plot, aes(x = WeekNR, y = AvgChoicePersistence_BSB)) + geom_line() + ylim(0,1) +  xlab("Week") + ylab("Average Choice Persistence") + 
  scale_x_continuous(breaks = c(1,20,40,60,70), labels = c("2019W1","2019W20","2019W40","2020W08", "2020W18")) + geom_vline(xintercept = 63) + geom_vline(xintercept = 67)

rm(list = c('post_analysis_choicepers', 'post_analysis_choicepers_plot', 'temp'))




### INCLUDE RETAILER IN BSB PERSISTENCE
# choice persistence metric (on week level)
TP_purch$Week <-strftime(TP_purch$Date_of_purchase, format = "%Y-W%V")
# aggregate data to week level
temp <- TP_purch %>% group_by(Panelist, Week) %>% summarize(n_bought = n(),BSBRetailer_bought = list(unique(BSBRetailer_clean)))
# re-order data
temp <- temp[order(temp$Panelist, temp$Week),]
# get values from previous purchase on the same row
temp <- temp %>% group_by(Panelist) %>% mutate(prev_BSBRetailer_bought = dplyr::lag(BSBRetailer_bought))
temp$prev_BSBRetailer_bought[temp$prev_BSBRetailer_bought == "NULL"] <- NA
# calculate choice persistence
temp <- temp %>% rowwise() %>% mutate(choicepersistence_BSBRetailer = (1/n_bought) * sum(BSBRetailer_bought %in% prev_BSBRetailer_bought) )
# calculate average across households
post_analysis_choicepers <- temp %>% group_by(Week) %>% summarise(AvgChoicePersistence_BSBRetailer = mean(choicepersistence_BSBRetailer) )
# average choice persistence of 2018 and 2019
mean(post_analysis_choicepers$AvgChoicePersistence_BSBRetailer[grepl("2018",post_analysis_choicepers$Week)|grepl("2019",post_analysis_choicepers$Week) ])
# min choice persistence of 2020
min(post_analysis_choicepers$AvgChoicePersistence_BSBRetailer[grepl("2020",post_analysis_choicepers$Week)])


# make plot
post_analysis_choicepers$Week_clean <- gsub("-W", "", post_analysis_choicepers$Week)
# only plot from 2019 onwards
post_analysis_choicepers_plot <- post_analysis_choicepers[!grepl("2018",post_analysis_choicepers$Week),]
post_analysis_choicepers_plot$WeekNR <- 1:length(unique(post_analysis_choicepers_plot$Week_clean))
# plot
ggplot(post_analysis_choicepers_plot, aes(x = WeekNR, y = AvgChoicePersistence_BSB)) + geom_line() + ylim(0,1) +  xlab("Week") + ylab("Average Choice Persistence") + 
  scale_x_continuous(breaks = c(1,20,40,60,70), labels = c("2019W1","2019W20","2019W40","2020W08", "2020W18")) + geom_vline(xintercept = 63) + geom_vline(xintercept = 67)

rm(list = c('post_analysis_choicepers', 'post_analysis_choicepers_plot', 'temp'))



# what makes the choice less persistent?
# different retailers?
# different prices?





### RETAILER PERSISTENCE ###

temp <- TP_purch %>% group_by(Panelist, Week) %>% summarize(n_visited = length(unique(Banner_name)), chains = list(unique(Banner_name)))
# re-order data
temp <- temp[order(temp$Panelist, temp$Week),]
# get values from previous purchase on the same row
temp <- temp %>% group_by(Panelist) %>% mutate(prev_chains = dplyr::lag(chains))
temp$prev_chains[temp$prev_chains == "NULL"] <- NA
# calculate choice persistence
temp <- temp %>% rowwise() %>% mutate(retailerpersistence = (1/n_visited) * sum(chains %in% prev_chains) )
# calculate average across households
avg_retailer_persistence <- temp %>% group_by(Week) %>% summarise(AvgRetailerPersistence = mean(retailerpersistence) )
# average choice persistence of 2018 and 2019
mean(avg_retailer_persistence$AvgRetailerPersistence[grepl("2018",avg_retailer_persistence$Week)|grepl("2019",avg_retailer_persistence$Week) ])
# min choice persistence of 2020
min(avg_retailer_persistence$AvgRetailerPersistence[grepl("2020",avg_retailer_persistence$Week)])

# make plot
avg_retailer_persistence$Week_clean <- gsub("-W", "", avg_retailer_persistence$Week)
# only plot from 2019 onwards
retailer_persistence_plot <- avg_retailer_persistence[!grepl("2018",avg_retailer_persistence$Week),]
retailer_persistence_plot$WeekNR <- 1:length(unique(retailer_persistence_plot$Week_clean))
# plot
ggplot(retailer_persistence_plot, aes(x = WeekNR, y = AvgRetailerPersistence)) + geom_line() +  ylim(0,1) +  xlab("Week") + ylab("Average Retailer Persistence") + 
  scale_x_continuous(breaks = c(1,20,40,60,70), labels = c("2019W1","2019W20","2019W40","2020W08", "2020W18")) + geom_vline(xintercept = 63) + geom_vline(xintercept = 67)

rm(list = c('avg_retailer_persistence', 'retailer_persistence_plot', 'temp'))

### NUMBER OF RETAILERS
### number of chains where toilet paper was bought each week
post_analysis_chain <- TP_purch %>% group_by(Panelist, Week) %>% summarize(chains = length(unique(Banner_name)))
# take average across households
post_analysis_chain <- post_analysis_chain %>% group_by(Week) %>% summarize(avg_n_chain = mean(chains))
summary(post_analysis_chain$avg_n_chain)
# make plot
post_analysis_chain$Week_clean <- gsub("-W", "", post_analysis_chain$Week)
# only plot from 2019 onwards
n_chains_plot <- post_analysis_chain[!grepl("2018",post_analysis_chain$Week),]
n_chains_plot$WeekNR <- 1:length(unique(n_chains_plot$Week_clean))
# plot
ggplot(n_chains_plot, aes(x = WeekNR, y = avg_n_chain))  + geom_line() + xlab("Week") + ylab("Average Number of Chains Visited") + ylim(0, 1.5) +
  scale_x_continuous(breaks = c(1,20,40,60,70), labels = c("2019W1","2019W20","2019W40","2020W08", "2020W18"))+ geom_vline(xintercept = 63) + geom_vline(xintercept = 67)


rm(list = c('n_chains_plot', 'post_analysis_chain'))







### PACKAGE SIZE PERSISTENCE ###

TP_purch$Week <-strftime(TP_purch$Date_of_purchase, format = "%Y-W%V")
# aggregate data to week level
temp <- TP_purch %>% group_by(Panelist, Week) %>% summarize(different_sizes_bought = length(unique(Volume_per_unit)), sizes_bought = list(unique(Volume_per_unit)))
# re-order data
temp <- temp[order(temp$Panelist, temp$Week),]
# get values from previous purchase on the same row
temp <- temp %>% group_by(Panelist) %>% mutate(prev_sizes_bought = dplyr::lag(sizes_bought))
temp$prev_sizes_bought[temp$prev_sizes_bought == "NULL"] <- NA
# calculate choice persistence
temp <- temp %>% rowwise() %>% mutate(packagepersistence = (1/different_sizes_bought) * sum(sizes_bought %in% prev_sizes_bought) )
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
package_persistence_plot$WeekNR <- 1:length(unique(package_persistence_plot$Week_clean))
# plot
ggplot(package_persistence_plot, aes(x = WeekNR, y = AvgPackagePersistence)) + geom_line() + ylim(0,1) +  xlab("Week") + ylab("Average Package Persistence") + 
  scale_x_continuous(breaks = c(1,20,40,60,70), labels = c("2019W1","2019W20","2019W40","2020W08", "2020W18"))+ geom_vline(xintercept = 63) + geom_vline(xintercept = 67)


rm(list = c('package_persistence_plot', 'avg_package_persistence', 'temp'))



### WHICH PACKAGE SIZE?

# plot with different package sizes as separate lines
temp <- TP_purch %>% group_by(Volume_per_unit, Week) %>% summarize(n_sold = sum(Total_unit_sales))
temp$Volume_per_unit <- factor(temp$Volume_per_unit)
temp$Week_clean <- gsub("-W", "", temp$Week)
# only plot from 2019 onwards
packagesizeplot <- temp[!grepl("2018",temp$Week),]
# plot
ggplot(packagesizeplot, aes(x = Week_clean, y = n_sold, group = Volume_per_unit)) + geom_line(aes(color = Volume_per_unit))

rm(list = c('temp', 'packagesizeplot'))



### PL PERSISTENCE ###

TP_purch$Week <-strftime(TP_purch$Date_of_purchase, format = "%Y-W%V")
TP_purch$PL <- TP_purch$PL.x
# aggregate data to week level
temp <- TP_purch %>% group_by(Panelist, Week) %>% summarize(PLNB_n = length(unique(PL)), PLNB_bought = list(unique(PL)))
# re-order data
temp <- temp[order(temp$Panelist, temp$Week),]
# get values from previous purchase on the same row
temp <- temp %>% group_by(Panelist) %>% mutate(prev_PLNB_bought = dplyr::lag(PLNB_bought))
temp$prev_PLNB_bought[temp$prev_PLNB_bought == "NULL"] <- NA
# calculate PL persistence
temp <- temp %>% rowwise() %>% mutate(PLpersistence = (1/PLNB_n) * sum(PLNB_bought %in% prev_PLNB_bought) )
# calculate average across households
avg_PL_persistence <- temp %>% group_by(Week) %>% summarise(AvgPLPersistence = mean(PLpersistence) )
# average PL persistence of 2018 and 2019
mean(avg_PL_persistence$AvgPLPersistence[grepl("2018",avg_PL_persistence$Week)|grepl("2019",avg_PL_persistence$Week) ])
# min choice persistence of 2020
min(avg_PL_persistence$AvgPLPersistence[grepl("2020",avg_PL_persistence$Week)])

# make plot
avg_PL_persistence$Week_clean <- gsub("-W", "", avg_PL_persistence$Week)
# only plot from 2019 onwards
PL_persistence_plot <- avg_PL_persistence[!grepl("2018",avg_PL_persistence$Week),]
PL_persistence_plot$WeekNR <- 1:length(unique(PL_persistence_plot$Week_clean))
# plot
ggplot(PL_persistence_plot, aes(x = WeekNR, y = AvgPLPersistence)) + geom_line() + ylim(0,1) +  xlab("Week") + ylab("Average PL Persistence") + 
  scale_x_continuous(breaks = c(1,20,40,60,70), labels = c("2019W1","2019W20","2019W40","2020W08", "2020W18"))+ geom_vline(xintercept = 63) + geom_vline(xintercept = 67)


rm(list = c('PL_persistence_plot', 'avg_PL_persistence', 'temp'))





################
# descriptives #
################


# PACKAGE SIZES
TP_purch$Volume_per_unit_factor <- factor(TP_purch$Volume_per_unit)
table(TP_purch$Volume_per_unit_factor)

smallpacks <- c("1","3","4","6","8", "9", "10")
mediumpacks <- c("12","16","18","20")
largepacks <- c("24", "30", "32", "40", "48","72")

TP_purch$PackageSize <- ifelse(TP_purch$Volume_per_unit_factor %in% smallpacks, "small", NA)
TP_purch$PackageSize <- ifelse(TP_purch$Volume_per_unit_factor %in% mediumpacks, "medium", TP_purch$PackageSize)
TP_purch$PackageSize <- ifelse(TP_purch$Volume_per_unit_factor %in% largepacks, "large", TP_purch$PackageSize)
table(TP_purch$PackageSize)

panic_period <- c("2020-W11", "2020-W12", "2020-W13", "2020-W14", "2020-W15")
year_before_period <- c("2019-W11", "2019-W12", "2019-W13", "2019-W14", "2019-W15")

packagesize <- TP_purch[TP_purch$Week %in% panic_period | TP_purch$Week %in% year_before_period,]
packagesize$period <- ifelse(packagesize$Week %in% panic_period, "panic", "year before")
packagesize <- packagesize %>% group_by(period, PackageSize) %>% summarise(MS = sum(Total_unit_sales))
packagesize <- packagesize %>% group_by(period) %>% mutate(totalunits = sum(MS))
packagesize$MS <- packagesize$MS / packagesize$totalunits
packagesize <- packagesize[order(packagesize$PackageSize, packagesize$period, decreasing =T),]


# PL versus NB
TP_purch$PL <- TP_purch$PL.y
TP_purch$PL <- ifelse(TP_purch$PL == "no","NB", "PL")
PLtest <- TP_purch[TP_purch$Week %in% panic_period | TP_purch$Week %in% year_before_period,]
PLtest$period <- ifelse(PLtest$Week %in% panic_period, "panic", "year before")
PLtest <- PLtest %>% group_by(period, PL) %>% summarise(MS_packs = sum(Total_unit_sales), MS_rolls = sum(Total_volume_sales))
PLtest <- PLtest %>% group_by(period) %>% mutate(totalunits = sum(MS_packs), totalrolls = sum(MS_rolls))
PLtest$MS_packs <- PLtest$MS_packs / PLtest$totalunits
PLtest$MS_rolls <- PLtest$MS_rolls / PLtest$totalrolls
PLtest <- PLtest[order(PLtest$PL, PLtest$period, decreasing = T),]







# average price paid
Pricetest <- TP_purch[TP_purch$Week %in% panic_period | TP_purch$Week %in% year_before_period,]
Pricetest$period <- ifelse(Pricetest$Week %in% panic_period, "panic", "year before")
Pricetest <- Pricetest %>% group_by(period) %>% summarise(avg_price_pack = mean(Total_value_sales/(Total_unit_sales*100)), avg_price_roll = mean(Total_value_sales/(Total_volume_sales*100)))



