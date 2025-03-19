##############################################
###                                        ###
###       incidence-quantity model         ###
###                                        ###
##############################################

rm(list=ls())

# set project_path
project_path <- here::here()

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
list.of.packages <- c("plyr","tidyr","ggplot2", "tidyverse", "zoo", "readxl", "car", "sampleSelection", "stargazer", "haven")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)

# create country list
country_list <- c("NL","BE","GE","FR","UK")
# set country
country <- country_list[country_list=="NL"]




# length of dataset
all.dates <- seq(as.Date("2018-01-01"),as.Date("2020-12-31"),by="days")






#############################################
# loading prepared toilet paper purchase data
#############################################

load(paste(project_path,"/INC-QUANT model/toilet paper/prepared_purchasedata_",country,sep=""))

# setting observation and initialization period
observation.dates <- seq(as.Date("2018-01-01"), as.Date("2020-05-01"), by="days")
initialization.dates <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by="days")

# get lists of households and subbrands
all.households <- unique(TP_purch$Panelist)
all.BSBretailers <- unique(TP_purch$BSBRetailer)

# restrict dataset to initialization period
TP_purch <- TP_purch[TP_purch$Date_of_purchase %in% initialization.dates,]



########################################
# loading purchase data (all categories)
########################################

all.purch.2018 <- read.csv(paste(data_path, "/",country, "/purchase_promo_2018.csv", sep = ""))
# restrict to initialization period and households of interest
all.purch.2018 <- all.purch.2018[all.purch.2018$Panelist %in% all.households & as.Date(all.purch.2018$Date_of_purchase) %in% initialization.dates,]

moderators <- all.purch.2018 %>% group_by(Panelist) %>% summarize(Total_budget = sum(Total_value_sales)/100)
rm(all.purch.2018)





#################################
# preparing consumer demographics
#################################

HH_char <- read.csv(paste(data_path,"/", country, "/panelist.csv", sep=""))
# use demographics of last quarter of initialization period
HH_char <- HH_char[HH_char$Panelist %in% all.households & HH_char$Quarter == "201804",]

if (country == "NL") {
HH_char$Age_OG <- factor(HH_char$Age,
                         levels = c(4,3,2,1,10,11,8,9,6,7,5),
                         labels = c("12-19year","20-24year","25-29year","30-34year","35-39year","40-44year","45-49year","50-54year","55-64year","65-74year","75+year"))
old_group <- c("75+year","55-64year","65-74year")
HH_char$Age <- ifelse(HH_char$Age_OG %in% old_group, 1, -1)


HH_char$Household_size_OG <- factor(HH_char$Household_size,
                                 levels=c(1:5), 
                                 labels=c("1 household member","2 household members","3 household members","4 household members", "5 or more household members"))
large_group <- c("3 household members","4 household members", "5 or more household members")
HH_char$Household_size <- ifelse(HH_char$Household_size_OG %in% large_group, 1, -1)


HH_char$Social_class <- factor(HH_char$Social_class, levels=c(1:6),labels=c("A","Unknown","D","C","B-minus","B-plus"))


HH_char$Income_class_year <- HH_char$Income_class
HH_char$Income_class <- factor(HH_char$Income_class,
                               levels=c(1,6,5,4,3,20,19,22,21,16,15,18,17,12,11,14,13,9,7),
                               labels=c("Below 700 euro","700-900 euro","900-1100 euro","1100-1300 euro","1300-1500 euro","1500-1700 euro","1700-1900 euro","1900-2100 euro",
                                        "2100-2300 euro","2300-2500 euro","2500-2700 euro","2700-2900 euro","2900-3100 euro","3100-3300 euro","3300-3500 euro","3500-3700 euro",
                                        "3700-3900 euro","3900-4100 euro","4100 euro or more"))
HH_char$Income_class_year <- factor(HH_char$Income_class_year,
                                    levels = c(1,6,5,4,3,20,19,22,21,16,15,18,17,12,11,14,13,9,7),
                                    labels=c("Below 8400 euro", "8400-10800 euro","10800-13200 euro", "13200-15600 euro", "15600-18000 euro", "18000-20400 euro", "20400-22800 euro",
                                             "22800-25200 euro", "25200-27600 euro", "27600-30000 euro", "30000-32400 euro", "32400-34800 euro", "34800-37200 euro", "37200-39600 euro",
                                             "39600-42000 euro", "42000-44400 euro", "44400-46800 euro", "46800-49200 euro", "49200 euro or more"))
# https://opendata.cbs.nl/#/CBS/nl/dataset/83932NED/table
above_median <- c("27600-30000 euro", "30000-32400 euro", "32400-34800 euro","34800-37200 euro", "37200-39600 euro","39600-42000 euro", "42000-44400 euro",
                  "44400-46800 euro", "46800-49200 euro", "49200 euro or more" )
below_median <- c("Below 8400 euro", "8400-10800 euro","10800-13200 euro", "13200-15600 euro", "15600-18000 euro", "18000-20400 euro", "20400-22800 euro",
                  "22800-25200 euro","25200-27600 euro")
HH_char$Income_above_median <- ifelse(HH_char$Income_class_year %in% above_median, 1, -1)

lowest_10pc <- c("Below 8400 euro", "8400-10800 euro","10800-13200 euro")
highest_10pc <- c("46800-49200 euro", "49200 euro or more")
middle <- c("13200-15600 euro", "15600-18000 euro", "18000-20400 euro", "20400-22800 euro",
            "22800-25200 euro", "25200-27600 euro", "27600-30000 euro", "30000-32400 euro", "32400-34800 euro", "34800-37200 euro", "37200-39600 euro",
            "39600-42000 euro", "42000-44400 euro", "44400-46800 euro")
HH_char$Income_dev1 <- ifelse(HH_char$Income_class_year %in% lowest_10pc, 2/3, -1/3)
HH_char$Income_dev2 <- ifelse(HH_char$Income_class_year %in% highest_10pc, 2/3, -1/3)

HH_char$Income_diff1 <- ifelse(HH_char$Income_class_year %in% lowest_10pc, -1, NA)
HH_char$Income_diff1 <- ifelse(HH_char$Income_class_year %in% middle, 1, HH_char$Income_diff1)
HH_char$Income_diff1 <- ifelse(HH_char$Income_class_year %in% highest_10pc, 0, HH_char$Income_diff1)
HH_char$Income_diff2 <- ifelse(HH_char$Income_class_year %in% lowest_10pc | HH_char$Income_class_year %in% middle, -0.5, 1)
}


# only keep the demographics we will use
moderators <- merge(moderators, HH_char[,c("Panelist","Age","Household_size","Income_above_median","Income_diff1","Income_diff2","Income_dev1","Income_dev2")], by= "Panelist", all.x=T)
rm(list = c('HH_char','old_group', 'large_group', 'lowest_10pc','highest_10pc','middle','above_median','below_median'))





##################################
# prepare consumer characteristics
##################################


### SHARE OF BUDGET ###
# total budget spent on toilet paper (+ 2 other measures needed for next moderators)
SoB <- TP_purch %>% group_by(Panelist) %>% summarise(TP_budget = sum(Total_value_sales)/100)
moderators <- merge(moderators, SoB, by="Panelist", all.x=T)
rm(SoB)
# SoB of toilet paper category = fraction of 2018 budget spent on toilet paper
moderators$SoB <- moderators$TP_budget / moderators$Total_budget



### PROMO SENSITIVITY ###
# # if multiple purchases on one day -> Promo_day > 1 if at least one purchase was Promo
TP_purch <- TP_purch %>% group_by(Panelist, Date_of_purchase) %>% mutate(Promo_day = sum(Promo))
# total budget and volume of promo toilet paper
Promo <- TP_purch %>% group_by(Panelist) %>% summarise(Promo_value = sum(Total_value_sales[Promo==1])/100, 
                                                       TP_volume = sum(Total_volume_sales), Promo_volume = sum(Total_volume_sales[Promo==1]),
                                                       TP_trips = length(unique(Date_of_purchase)), Promo_trips = length(unique(Date_of_purchase[Promo_day>=1])))
moderators <- merge(moderators, Promo, by="Panelist", all.x=T)
rm(Promo)
# Promo sensitivity = fraction of toilet paper budget spent on promo    &     fraction of toilet paper rolls bought in promo
moderators$Promo.sens_value <- moderators$Promo_value / moderators$TP_budget
moderators$Promo.sens_volume <- moderators$Promo_volume / moderators$TP_volume
moderators$Promo.sens_trip <- moderators$Promo_trips / moderators$TP_trips



### PRIVATE LABEL SHARE ###
TP_purch$PL <- TP_purch$PL.x
# # if multiple purchases on one day -> PL_day > 1 if at least one purchase was PL
TP_purch <- TP_purch %>% group_by(Panelist, Date_of_purchase) %>% mutate(PL_day = sum(PL))
# total budget and volume of PL toilet paper
PL <- TP_purch %>% group_by(Panelist) %>% summarise(PL_value = sum(Total_value_sales[PL==1])/100, 
                                                    PL_volume = sum(Total_volume_sales[PL==1]), 
                                                    PL_trips = length(unique(Date_of_purchase[PL_day>=1])))
moderators <- merge(moderators, PL, by="Panelist", all.x=T)
rm(PL)
# PL share = fraction of toilet paper budget spent on PL    &    fraction of toilet paper rolls bought as PL
moderators$PL.share_value <- moderators$PL_value / moderators$TP_budget
moderators$PL.share_volume <- moderators$PL_volume / moderators$TP_volume
moderators$PL.share_trip <- moderators$PL_trips / moderators$TP_trips



### BRAND LOYALTY ###
Loyalty <- TP_purch %>% group_by(Panelist, BrandSubBrand) %>% count(BrandSubBrand)
# # count number of different sub-brands purchased
# Loyalty <- Loyalty %>% group_by(Panelist) %>% count(Panelist)
# Loyalty$number.SB <- Loyalty$n
# moderators <- merge(moderators, Loyalty[,c("Panelist","number.SB")], by="Panelist", all.x=T)
# Herfindahl inspired Brand Support Index
Loyalty$numerator <- Loyalty$n ^2
Loyalty <- Loyalty %>% group_by(Panelist) %>% mutate(denominator = sum(n)^2)
Loyalty$temp <- (Loyalty$numerator / Loyalty$denominator) * log(Loyalty$n)
Loyalty <- Loyalty %>% group_by(Panelist) %>% summarise(BrandSupport = sum(temp))
Loyalty$BrandSupport_std <- (Loyalty$BrandSupport - min(Loyalty$BrandSupport)) / (max(Loyalty$BrandSupport) - min(Loyalty$BrandSupport))
moderators <- merge(moderators, Loyalty[,c("Panelist","BrandSupport", "BrandSupport_std")], by="Panelist", all.x=T)
rm(Loyalty)



### FAMILIARITY ###
# aggregate data to daily level:  if HH buys different types of toilet paper within same day, IPT would otherwise be 0 (biases the mean)
TP_purch_agg <- ddply(TP_purch, .(Date_of_purchase, Panelist), summarise, Total_volume_sales = sum(Total_volume_sales))
TP_purch_agg <- TP_purch_agg[order(TP_purch_agg$Panelist, TP_purch_agg$Date_of_purchase),]
# calculate number of days since previous purchase (IPT)
TP_purch_agg$TimeSincePrevPurch <- do.call(c,by(TP_purch_agg$Date_of_purchase,TP_purch_agg$Panelist,function(x) c(NA,diff(x))))
# take average of this IPT + calculate average Q
Familiarity <- TP_purch_agg %>% group_by(Panelist) %>%  summarize(AvgIPT = mean(TimeSincePrevPurch, na.rm=T), AvgQ = mean(Total_volume_sales, na.rm=T))
moderators <- merge(moderators, Familiarity, by="Panelist", all.x=T)
rm(Familiarity)

# missing values for AvgIPT = households who only buy toilet paper once in 2018 (not for AvgQ)
# should be excluded from data, so that N is the same for incidence and quantity model
excl.households <- moderators$Panelist[is.na(moderators$AvgIPT)]
moderators <- moderators[!moderators$Panelist %in% excl.households, ]
TP_purch <- TP_purch[!TP_purch$Panelist %in% excl.households, ]
all.households <- unique(TP_purch$Panelist)

rm(list=c('TP_purch','TP_purch_agg'))



### AVG INVENTORY ###
load(paste(project_path,"/Preparation/toilet paper inventory/",country,"_INV_toiletpaper.RData",sep=""))
dates_2019 <- seq(from=as.Date("2019-01-01"), to=as.Date("2019-12-31"), by="days")
Inv <- Inv %>% group_by(ID) %>% summarize(AvgInv = mean(Inv[Date %in% dates_2019]))
moderators <- merge(moderators, Inv, by.x="Panelist", by.y="ID")
rm(list=c('Inv','dates_2019'))





######################################
# mean-centering continuous moderators
######################################
moderators$Total_budget_MC <- moderators$Total_budget - mean(moderators$Total_budget)

moderators$TP_budget_MC <- moderators$TP_budget - mean(moderators$TP_budget)

moderators$SoB_MC <- moderators$SoB - mean(moderators$SoB)

moderators$Promo.sens_value_MC <- moderators$Promo.sens_value - mean(moderators$Promo.sens_value)
moderators$Promo.sens_volume_MC <- moderators$Promo.sens_volume - mean(moderators$Promo.sens_volume)
moderators$Promo.sens_trip_MC <- moderators$Promo.sens_trip - mean(moderators$Promo.sens_trip)

moderators$PL.share_value_MC <- moderators$PL.share_value - mean(moderators$PL.share_value)
moderators$PL.share_volume_MC <- moderators$PL.share_volume - mean(moderators$PL.share_volume)
moderators$PL.share_trip_MC <- moderators$PL.share_trip - mean(moderators$PL.share_trip)

#moderators$number.SB <- moderators$number.SB - mean(moderators$number.SB)
moderators$BrandSupport_MC <- moderators$BrandSupport - mean(moderators$BrandSupport)
moderators$BrandSupport_std_MC <- moderators$BrandSupport_std - mean(moderators$BrandSupport_std)

moderators$AvgIPT_MC <- moderators$AvgIPT - mean(moderators$AvgIPT)
moderators$AvgQ_MC <- moderators$AvgQ - mean(moderators$AvgQ)

moderators$AvgInv_MC <- moderators$AvgInv - mean(moderators$AvgInv)




##########################
# check summary statistics
##########################

# promotional sensitivity
summary(moderators$Promo.sens_value_MC)
summary(moderators$Promo.sens_volume_MC)
summary(moderators$Promo.sens_trip_MC)
# private label share
summary(moderators$PL.share_value_MC)
summary(moderators$PL.share_volume_MC)
summary(moderators$PL.share_trip_MC)
# brand loyalty / taste for variety
#summary(moderators$number.SB)
summary(moderators$BrandSupport_MC)
# category familiarity
summary(moderators$AvgIPT_MC)
summary(moderators$AvgQ_MC)
# inventory
summary(moderators$AvgInv_MC)
# budget
summary(moderators$Total_budget_MC)
# share of budget of category
summary(moderators$SoB_MC)
# age
table(moderators$Age)
# income
table(moderators$Income_above_median)
table(moderators$Income_diff1)
table(moderators$Income_diff2)
table(moderators$Income_dev1)
table(moderators$Income_dev2)

# household size
table(moderators$Household_size)






####################################################################################
# exclude households with missing AvgIPT from original toilet paper purchase dataset
####################################################################################

# to ensure n is the same for all models
load(paste(project_path,"/INC-QUANT model/toilet paper/prepared_purchasedata_",country,sep=""))
TP_purch <- TP_purch[!TP_purch$Panelist %in% excl.households, ]
save(TP_purch, file=paste(project_path,"/INC-QUANT model/toilet paper/prepared_purchasedata_NL_final",sep=""))



save(moderators, file=paste(project_path,"/INC-QUANT model/toilet paper/prepared_moderators_NL",sep=""))
