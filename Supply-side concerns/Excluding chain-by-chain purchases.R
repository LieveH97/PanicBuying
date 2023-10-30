##############################################
###                                        ###
###       JUMBO PURCHASE LIMIT             ###
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







##########################################
#### DROPPING ALL PURCHASES OF CHAIN X ###
##########################################

load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/PricePromoIndex_NL.RData",sep=""))
PricePromoIndex <- final_PricePromoIndex
rm(final_PricePromoIndex)
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/prepared_purchasedata_NL_final",sep=""))




#TP_purch <- TP_purch[TP_purch$Banner_name != "Jumbo", ]
#TP_purch <- TP_purch[TP_purch$Banner_name != "Albert Heijn", ]
#TP_purch <- TP_purch[TP_purch$Banner_name != "Plus", ]
#TP_purch <- TP_purch[TP_purch$Banner_name != "Lidl", ]
#TP_purch <- TP_purch[TP_purch$Banner_name != "Aldi", ]
TP_purch <- TP_purch[TP_purch$Banner_name != "DIRK", ]






### MINIMUM PURCHASE FILTER ###
# only keep households that make a toilet paper purchase at least once every year of 2018-2020

filter_minpurchase_year <- function(df, yearlist) {
  # create dataframe with all panelist ID and a flag
  panelists <- data.frame(ID=unique(df$Panelist),filterOK=NA)
  # aggregate purchases up to panelist-year level
  df <- ddply(df,.(Panelist, Year), summarise, Total_unit_sales=sum(Total_unit_sales),
              Total_value_sales=sum(Total_value_sales))
  # for every panelist --> make sure as many unique (because ddply) years as in list
  for (i in seq_along(panelists$ID)) {
    panelists$filterOK[i] <- ifelse(sum(df$Year[df$Panelist==panelists$ID[i]] %in% yearlist)==length(yearlist),1,0) }
  return(panelists)
}
yearlist <- c("2018","2019","2020")
filter_minpurch <- filter_minpurchase_year(TP_purch,yearlist)
filter_minpurch <- filter_minpurch[filter_minpurch$filterOK==1,]
# apply filter to TP_purch dataset
TP_purch <- TP_purch[TP_purch$Panelist %in% filter_minpurch$ID,]



### INTERPURCHASE TIME FILTER ###
# exclude households whose maximum interpurchase time up to 1/1/2021 is larger than the mean + 2 standard deviations

#first aggregate up to Day-Household level
#TP_purch_agg_d <- ddply(TP_purch,.(Panelist,Date_of_purchase, Quarter, Year),
#                        summarise,tot_sheet = sum(tot_sheet))
TP_purch_agg_d <- TP_purch %>% group_by(Panelist,Date_of_purchase, Quarter, Year)%>% summarize(tot_rolls = sum(Total_volume_sales))
# calculate interpurchase time between every purchase
TP_purch_agg_d <- TP_purch_agg_d[order(TP_purch_agg_d$Panelist,TP_purch_agg_d$Date_of_purchase),]
TP_purch_agg_d$IPT<-do.call(c,by(TP_purch_agg_d$Date_of_purchase,TP_purch_agg_d$Panelist,function(x) c(NA,diff(x))))
# calculate each household's average interpurchase time
dsummary_2020 <- function(df) {
  df %>% group_by(flag) %>% summarize("Min."=min(max_IPT), "First Qu."=quantile(max_IPT,0.25),"Median"=median(max_IPT), "Mean"=mean(max_IPT),
                                      "Third Qu."= quantile(max_IPT,0.75), "Max."=max(max_IPT), "St.Dev"=sd(max_IPT)) }
TP_purch_agg_d$flag <- ifelse(TP_purch_agg_d$Date_of_purchase < as.Date("2020-01-01"), "before", "during/after")
# add December 31, 2019 as a zero-purchase in TP_purch_agg_d
# TP_purch_agg_d_cut <- rbind(TP_purch_agg_d,  cbind(Panelist=as.character(unique(TP_purch_agg_d$Panelist)),
#                                                Date_of_purchase="2019-12-31", Quarter="201904", Year="2019",
#                                                tot_sheet=0,IPT=NA,flag="before"))
TP_purch_agg_d_cut <- rbind(TP_purch_agg_d,  data.frame(Panelist=as.character(unique(TP_purch_agg_d$Panelist)),
                                                   Date_of_purchase=as.Date("2019-12-31"), Quarter="201904", Year="2019",
                                                   tot_rolls=0,IPT=NA,flag="before"))
TP_purch_agg_d_cut <- TP_purch_agg_d_cut[order(TP_purch_agg_d_cut$Panelist,TP_purch_agg_d_cut$Date_of_purchase),]
TP_purch_agg_d_cut$IPT_cut <- do.call(c,by(TP_purch_agg_d_cut$Date_of_purchase,TP_purch_agg_d_cut$Panelist,function(x) c(NA,diff(x))))
# add flag
TP_purch_agg_d_cut$flag <- ifelse(TP_purch_agg_d_cut$Date_of_purchase < as.Date("2020-01-01"), "before2020", "2020")
max_IPT <-aggregate(IPT_cut ~ Panelist + flag, TP_purch_agg_d_cut, max)
colnames(max_IPT) <- c("Panelist", "flag", "max_IPT")
max_IPT_before2020 <- subset(max_IPT, flag=="before2020")
# filter: mean +/- 2 st.dev.
filter_IPT_2020_st <- max_IPT_before2020[max_IPT_before2020$max_IPT < mean(max_IPT_before2020$max_IPT)+2*sd(max_IPT_before2020$max_IPT), ]
# apply the filter
panelists_keep_ID_cat <- as.vector(filter_IPT_2020_st$Panelist)
TP_purch <- TP_purch[TP_purch$Panelist %in% panelists_keep_ID_cat,]

# for each household, we no longer make a distinction between different products; just whether they bought and if so, how many rolls
TP_purch <- TP_purch %>% group_by(Date_of_purchase, Panelist) %>% summarise(Total_volume_sales = sum(Total_volume_sales))

# filtered households should also be excluded from PricePromoIndex dataframe, as this is basis for FullData
all.dates <- unique(TP_purch$Date_of_purchase)
all.households <- unique(TP_purch$Panelist)
estimation.dates <- seq.Date(from=as.Date("2019-01-01"), to=as.Date("2020-04-30"), by="days")
PricePromoIndex <- PricePromoIndex[PricePromoIndex$Household %in% all.households,]


### CREATE THE FINAL DATASET

# dimensions: #dates X #households
# for every date, for every households: do they buy, and if yes, how much
FullData <- PricePromoIndex
rm(PricePromoIndex)

# include purchase info
FullData <- merge(FullData, TP_purch, by.x=c("Date","Household"), by.y=c("Date_of_purchase","Panelist"), all.x=T)
rm(TP_purch)

# purchase: yes or no
FullData$PurchaseFlag <- ifelse(!is.na(FullData$Total_volume_sales), 1, 0)

# estimation period
FullData <- FullData[FullData$Date %in% estimation.dates,]

library(lubridate)
# day of the week dummies
FullData$Weekday <- wday(FullData$Date, week_start = 1)
FullData$Weekday <- factor(FullData$Weekday)
# week of the year dummies
FullData$Week <- week(FullData$Date)
FullData$Week <- factor(FullData$Week)
# month of the year dummies
FullData$Month <- month(FullData$Date)
FullData$Month <- factor(FullData$Month)
# quarter of the year dummies
FullData$Quarter <- quarter(FullData$Date)
FullData$Quarter <- factor(FullData$Quarter)
# year
FullData$Year <- year(FullData$Date)
FullData$Year <- factor(FullData$Year)

# INVENTORY
load(paste(project_path,"/R_files/Preparation/toilet paper inventory/",country,"_INV_toiletpaper.RData",sep=""))
FullData <- merge(FullData, Inv, by.x=c("Date","Household"), by.y=c("Date", "ID"))
rm(Inv)
# mean centering inventory with household mean of 2019 and 2020
FullData <- FullData %>% group_by(Household) %>% mutate(meanINV = mean(Inv))
FullData$HHInv <- FullData$Inv - FullData$meanINV
col_remove <- c("meanINV")
FullData <- FullData[,!colnames(FullData) %in% col_remove]
rm(col_remove)

# PANIC INDEX
panic <- data.frame(Date = estimation.dates)
temp <- read_dta(file="C:/PhD KU Leuven/OneDrive - KU Leuven/Projects_Personal/Project 1 - Stockpiling vs Panic Buying/Panic_COVID19_Data/Panic_Index_Data.dta")
temp <- temp[temp$geoCode == "NL",]
temp$Date <- as.Date(temp$time)
panic <- merge(panic, temp, by="Date", all.x=T)
panic <- panic[, c("Date", "panic_index")]
rm(temp)
# NEW VERSION: impute 2019 and scale between 0 and 1 (like PromoIndex)
# calculate mean panic index of January 2020
panic_jan2020 <- mean(panic$panic_index[panic$Date %in% seq.Date(from=as.Date("2020-01-01"), to=as.Date("2020-01-31"), by="days")])
# for 2019 (no panic index available): impute mean panic index of January 2020
panic$panic_index[is.na(panic$panic_index)] <- panic_jan2020
rm(panic_jan2020)
# rescale panic index to range between 0 and 1
min_panic <- min(panic$panic_index)
max_panic <- max(panic$panic_index)
panic$panic_index_rescaled <- (panic$panic_index - min_panic) / (max_panic - min_panic)
summary(panic$panic_index_rescaled)
# add to FullData
FullData <- merge(FullData, panic, by="Date", all = T)
FullData$PanicIndex <- FullData$panic_index_rescaled
rm(list=c('panic','min_panic','max_panic'))
col_remove <- c("panic_index", "panic_index_rescaled")
FullData <- FullData[,!colnames(FullData) %in% col_remove]
rm(col_remove)

# MODERATORS
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/prepared_moderators_NL",sep=""))
FullData <- merge(FullData, moderators, by.x="Household", by.y="Panelist", all.x=T)
rm(moderators)
col_remove <- c("TP_volume", "Promo_value","Promo_volume","PL_value","PL_volume")
FullData <- FullData[,!colnames(FullData) %in% col_remove]
rm(col_remove)

### RENAME THE VARIABLES FOR CLEAR INTERPRETATION ###
colnames(FullData)[which(names(FullData) == "ShelfPriceIndex")] <- "PriceIndex"
colnames(FullData)[which(names(FullData) == "HHInv")] <- "HouseholdInventory"
colnames(FullData)[which(names(FullData) == "Inv")] <- "HouseholdInventory_nonMC"
colnames(FullData)[which(names(FullData) == "AvgIPT_MC")] <- "Average_IPT"
colnames(FullData)[which(names(FullData) == "AvgIPT")] <- "Average_IPT_nonMC"
colnames(FullData)[which(names(FullData) == "AvgQ_MC")] <- "Average_Q"
colnames(FullData)[which(names(FullData) == "AvgQ")] <- "Average_Q_nonMC"
colnames(FullData)[which(names(FullData) == "Promo.sens_volume_MC")] <- "PromoSensitivity"
colnames(FullData)[which(names(FullData) == "Promo.sens_volume")] <- "PromoSensitivity_nonMC"
colnames(FullData)[which(names(FullData) == "PL.share_volume_MC")] <- "PLShare"
colnames(FullData)[which(names(FullData) == "PL.share_volume")] <- "PLShare_nonMC"
colnames(FullData)[which(names(FullData) == "BrandSupport_MC")] <- "BrandLoyalty"
colnames(FullData)[which(names(FullData) == "BrandSupport")] <- "BrandLoyalty_nonMC"
colnames(FullData)[which(names(FullData) == "Income_above_median")] <- "Income"
colnames(FullData)[which(names(FullData) == "Weekday")] <- "Day_of_week"
colnames(FullData)[which(names(FullData) == "Household_size")] <- "HouseholdSize"




#####################################
### SAVE NEW VERSION OF FULL DATA ###
#####################################

#save(FullData, file=paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_Jumbo.RData",sep=""))
#save(FullData, file=paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_AlbertHeijn.RData",sep=""))
#save(FullData, file=paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_Plus.RData",sep=""))
#save(FullData, file=paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_Lidl.RData",sep=""))
#save(FullData, file=paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_Aldi.RData",sep=""))
save(FullData, file=paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_DIRK.RData",sep=""))








#######################
# RUN TWO-STAGE MODEL #
#######################

#load(paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_Jumbo.RData",sep=""))
#load(paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_AlbertHeijn.RData",sep=""))
#load(paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_Plus.RData",sep=""))
#load(paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_Lidl.RData",sep=""))
#load(paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_Aldi.RData",sep=""))
load(paste(project_path,"/R_files/Supply-side concerns/FullData_excluding_DIRK.RData",sep=""))

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

# two-stage model on data excluding Jumbo purchases
heckman_main <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_IPT 
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                          + HouseholdInventory + Average_Q   
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          data = FullData)
# clean version: excluding time fixed effects from output
keep <- c("PromoIndex","PanicIndex","HouseholdInventory","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")

# save clean output
#two_stages_output_clean(heckman_main, "Model1_excl_Jumbo_purchases", keep)
#two_stages_output_clean(heckman_main, "Model1_excl_AH_purchases", keep)
#two_stages_output_clean(heckman_main, "Model1_excl_Plus_purchases", keep)
#two_stages_output_clean(heckman_main, "Model1_excl_Lidl_purchases", keep)
#two_stages_output_clean(heckman_main, "Model1_excl_Aldi_purchases", keep)
two_stages_output_clean(heckman_main, "Model1_excl_DIRK_purchases", keep)

rm(heckman_main)
