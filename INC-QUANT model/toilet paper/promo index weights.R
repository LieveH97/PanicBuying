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

### purchase data ###
load(paste(project_path,"/R_files/Preparation/toilet paper/",country,"_purch_toiletpaper.RData",sep=""))





############################
# setting observation window
############################

observation.dates <- seq(as.Date("2018-01-01"), as.Date("2020-04-30"), by="days")
TP_purch <- TP_purch[TP_purch$Date_of_purchase %in% observation.dates,]





#######################
# investigate retailers
#######################

chain.info <- read.csv(paste(data_path,"/",country,"/shopcode.csv",sep=""))
chain.info$Local_store_type <- factor(chain.info$Local_store_type)
table(chain.info$Local_store_type)

# types of stores:
    # undefined                  3
    # Overige aankoopplaatsen    152
    # Totaal discounters         10
    # Totaal middensupers        46
    # Totaal servicesupers       20

# include this store type in purchase data
TP_purch <- merge(TP_purch, chain.info, by="Banner_name")


# group together different formats of same chain
AlbertHeijn_formats <- c("Albert Heijn", "AH XL", "AH to Go", "Albert.nl")
Coop_formats <- c("Coop", "Coop Compact", "Coop Vandaag", "Coop-Coop Compact", "SuperCoop")
Jumbo_formats <- c("Jumbo", "Jumbo City", "Jumbo Foodmarkt")
Spar_formats <- c("Spar", "Spar Express","Spar City", "Spar Voordeelmarkt")

TP_purch$Banner_name[TP_purch$Banner_name %in% AlbertHeijn_formats] <- AlbertHeijn_formats[1]
TP_purch$Banner_name[TP_purch$Banner_name %in% Coop_formats] <- Coop_formats[1]
TP_purch$Banner_name[TP_purch$Banner_name %in% Jumbo_formats] <- Jumbo_formats[1]
TP_purch$Banner_name[TP_purch$Banner_name %in% Spar_formats] <- Spar_formats[1]


TP_purch$Banner_name <- factor(TP_purch$Banner_name)
table(TP_purch$Banner_name)






######################################
# calculate market shares of retailers
######################################

all.chains <- unique(TP_purch$Banner_name)
all.chains <- sort(all.chains)

#MS per chain across all three years
chain_MS_across_years <- ddply(TP_purch, .(Banner_name), summarise, Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
chain_MS_across_years$TotalValue <- sum(TP_purch$Total_value_sales)
chain_MS_across_years$TotalVolume <- sum(TP_purch$Total_volume_sales)
chain_MS_across_years$MS_Val <- chain_MS_across_years$Total_value_sales / chain_MS_across_years$TotalValue
chain_MS_across_years$MS_Vol <- chain_MS_across_years$Total_volume_sales / chain_MS_across_years$TotalVolume
chain_MS_across_years <- chain_MS_across_years[order(chain_MS_across_years$Banner_name),]

#MS per chain per year
chain_MS <- ddply(TP_purch, .(Year, Banner_name), summarise, Tot_value_sales = sum(Total_value_sales), Tot_vol_sales = sum(Total_volume_sales))
year_sum <- ddply(TP_purch, .(Year), summarise, Tot_value_sales = sum(Total_value_sales), Tot_vol_sales = sum(Total_volume_sales))
for (i in seq_along(chain_MS$Banner_name)) { chain_MS$YearTotal_Val[i] <- year_sum$Tot_value_sales[chain_MS$Year[i] == year_sum$Year]
                                             chain_MS$YearTotal_Vol[i] <- year_sum$Tot_vol_sales[chain_MS$Year[i] == year_sum$Year]    }
chain_MS$MS_Val <- chain_MS$Tot_value_sales / chain_MS$YearTotal_Val
chain_MS$MS_Vol <- chain_MS$Tot_vol_sales / chain_MS$YearTotal_Vol
chain_MS <- reshape(chain_MS, idvar = "Banner_name", timevar = "Year", direction = "wide")
chain_MS[, c(6,7,12,13,18,19)][is.na(chain_MS[, c(6,7,12,13,18,19)])] <- 0
chain_MS <- chain_MS[order(chain_MS$Banner_name),]
chain_MS$MS.overall_Val <- chain_MS_across_years$MS_Val
chain_MS$MS.overall_Vol <- chain_MS_across_years$MS_Vol
chain_MS <- chain_MS[order(chain_MS$MS.overall_Val, decreasing = T),]
chain_MS$CumMS.overall_Val <- cumsum(chain_MS$MS.overall_Val)
chain_MS$CumMS.overall_Vol <- cumsum(chain_MS$MS.overall_Vol)
chain_MS$CumMS_Val.2018 <- cumsum(chain_MS$MS_Val.2018)
chain_MS$CumMS_Val.2019 <- cumsum(chain_MS$MS_Val.2019)
chain_MS$CumMS_Val.2020 <- cumsum(chain_MS$MS_Val.2020)
chain_MS$CumMS_Vol.2018 <- cumsum(chain_MS$MS_Vol.2018)
chain_MS$CumMS_Vol.2019 <- cumsum(chain_MS$MS_Vol.2019)
chain_MS$CumMS_Vol.2020 <- cumsum(chain_MS$MS_Vol.2020)

#combine overall and yearly market shares into one dataframe
final_chain_MS <- chain_MS[, c("Banner_name", "MS.overall_Val","CumMS.overall_Val","MS.overall_Vol","CumMS.overall_Vol", "MS_Val.2018","CumMS_Val.2018", "MS_Vol.2018","CumMS_Vol.2018",
                               "MS_Val.2019","CumMS_Val.2019","MS_Vol.2019","CumMS_Vol.2019","MS_Val.2020","CumMS_Val.2020","MS_Vol.2020","CumMS_Vol.2020")]
row.names(final_chain_MS) <- seq_along(final_chain_MS$Banner_name)

   

    # top 6 seems like good cut-off point
    # this way, we avoid Kruidvat (a drugstore) to be a focal retailer
    # every year, at least 3.2% market share in value and at least 3.3% market share in volume
    # cumulatively, every year more than 76.5% market share in volume and more than 78.6% in value



# select which ones to study individually
chains.keep <- final_chain_MS$Banner_name[1:6]
TP_purch$Banner_name <- ifelse(TP_purch$Banner_name %in% chains.keep, as.character(TP_purch$Banner_name), "RestRetailer")

rm(list=c("chain_MS","chain_MS_across_years","chain.info","final_chain_MS"))







###################################################
# preparing sub-brands-retailer as unit of analysis
###################################################

SKU.info <- read.csv(paste(data_path,"/",country,"/barcode.csv",sep=""))
TP_purch <- merge(TP_purch, SKU.info, by= "Barcode")
rm(SKU.info)

# it seems that some brands don't have subbrand -> subbrand variable is then missing
table(TP_purch$Sub_brand)

# create new variable that combines brand and subbrand
TP_purch$BrandSubBrand <- paste(TP_purch$Brand, TP_purch$Sub_brand, sep= " ")

# aggregation level of analysis = BrandSubBrand-Retailer
# create new variable for this
TP_purch$BSBRetailer <- paste(TP_purch$Banner_name, TP_purch$BrandSubBrand, sep=" ")


# 114 combinations
length(table(TP_purch$BSBRetailer))







####################################################
# calculate market shares of sub-brands per retailer
####################################################

# Market shares across all three years (within retailer)
subbrands_MS_across_years <- ddply(TP_purch, .(BrandSubBrand, Banner_name), summarize, Tot_vol_sales = sum(Total_volume_sales), Tot_val_sales = sum(Total_value_sales))
subbrands_MS_across_years <- subbrands_MS_across_years %>% group_by(Banner_name) %>% mutate(TotalVolume = sum(Tot_vol_sales), TotalValue = sum(Tot_val_sales)) %>% ungroup()
subbrands_MS_across_years <- subbrands_MS_across_years %>% group_by(Banner_name, BrandSubBrand) %>% mutate(MS_Volume = Tot_vol_sales / TotalVolume, MS_Value = Tot_val_sales / TotalValue)
subbrands_MS_across_years$BSBRetailer <- paste(subbrands_MS_across_years$Banner_name, subbrands_MS_across_years$BrandSubBrand, sep=" ") 


#Market shares of sub-brand per year (within retailer)
subbrands_MS <- ddply(TP_purch, .(BrandSubBrand, Banner_name, Year), summarize, Tot_vol_sales = sum(Total_volume_sales), Tot_val_sales = sum(Total_value_sales))
subbrands_MS <- subbrands_MS %>% group_by(Banner_name, Year) %>% mutate(TotalVolume = sum(Tot_vol_sales), TotalValue = sum(Tot_val_sales)) %>% ungroup()
subbrands_MS <- subbrands_MS %>% group_by(Banner_name, Year, BrandSubBrand) %>% mutate(MS_Volume = Tot_vol_sales / TotalVolume, MS_Value = Tot_val_sales / TotalValue)
subbrands_MS$BSBRetailer <- paste(subbrands_MS$Banner_name, subbrands_MS$BrandSubBrand, sep=" ")
subbrands_MS <- subbrands_MS[, c("BSBRetailer","Banner_name","Year", "MS_Volume","MS_Value")]
subbrands_MS$Year <- as.character(subbrands_MS$Year)
subbrands_MS <- reshape(as.data.frame(subbrands_MS), timevar = "Year", idvar = "BSBRetailer", direction = "wide", v.names = c("MS_Volume", "MS_Value"))
subbrands_MS[,c(3:8)][is.na(subbrands_MS[,c(3:8)])] <- 0
if (length(subbrands_MS$BSBRetailer == subbrands_MS_across_years$BSBRetailer) == length(subbrands_MS$BSBRetailer)) {
subbrands_MS$MS_Volume.overall <- subbrands_MS_across_years$MS_Volume
subbrands_MS$MS_Value.overall <- subbrands_MS_across_years$MS_Value  }
# cumulative sums per retailer
subbrands_MS <- subbrands_MS %>% group_by(Banner_name) %>% arrange(desc(MS_Value.overall)) %>% mutate(CumMS_Volume.overall = cumsum(MS_Volume.overall), CumMS_Value.overall = cumsum(MS_Value.overall),
                                                                  CumMS_Volume.2018 = cumsum(MS_Volume.2018), CumMS_Value.2018 = cumsum(MS_Value.2018),
                                                                  CumMS_Volume.2019 = cumsum(MS_Volume.2019), CumMS_Value.2019 = cumsum(MS_Value.2019),
                                                                  CumMS_Volume.2020 = cumsum(MS_Volume.2020), CumMS_Value.2020 = cumsum(MS_Value.2020),)
subbrands_MS <- subbrands_MS[,c(1,2,9,11,10,12,5,13,6,14,7,15,8,16,3,17,4,18)]


#Investigate each retailer separately
subbrands_MS_per_retailer <- dlply(subbrands_MS, .(Banner_name))

   # Albert Heijn
   subbrands_MS_per_retailer$`Albert Heijn` <- subbrands_MS_per_retailer$`Albert Heijn`[order(subbrands_MS_per_retailer$`Albert Heijn`$MS_Value.overall, decreasing=T),]
   subbrands_MS_per_retailer$`Albert Heijn`
   
      # cut off at 5 sub-brands
      # min.3.6% MS in Value and 4.1% in Volume across three years
      # every year, at least 2.8% in Volume and at least 2.9% in Value
      # cumulatively, every year more than 95.5% market share in Volume and 94% in Value
   
   BSB.keep_AH <- subbrands_MS_per_retailer$`Albert Heijn`$BSBRetailer[1:5]
   TP_purch$BSBRetailer[TP_purch$Banner_name == "Albert Heijn"] <- ifelse(TP_purch$BSBRetailer[TP_purch$Banner_name == "Albert Heijn"] %in% BSB.keep_AH, 
                                                                          TP_purch$BSBRetailer[TP_purch$Banner_name == "Albert Heijn"], "Albert Heijn RestBSB")
   
   
   #Aldi
   subbrands_MS_per_retailer$Aldi <- subbrands_MS_per_retailer$Aldi[order(subbrands_MS_per_retailer$Aldi$MS_Value.overall, decreasing =T),]
   subbrands_MS_per_retailer$Aldi
   
     # only 5 subbrands at Aldi, of which one has 99+% market share
   
   BSB.keep_Aldi <- subbrands_MS_per_retailer$Aldi$BSBRetailer[1]
   TP_purch$BSBRetailer[TP_purch$Banner_name == "Aldi"] <- ifelse(TP_purch$BSBRetailer[TP_purch$Banner_name == "Aldi"] %in% BSB.keep_Aldi,
                                                                  TP_purch$BSBRetailer[TP_purch$Banner_name == "Aldi"], "Aldi RestBSB")


   #DIRK
   subbrands_MS_per_retailer$DIRK <- subbrands_MS_per_retailer$DIRK[order(subbrands_MS_per_retailer$DIRK$MS_Value.overall, decreasing=T),]
   subbrands_MS_per_retailer$DIRK
   
     # cut off at 8 sub-brands
     # min.5.2% MS in Value and 5.2% in Volume across three years
     # a lot of listings and delistings (so 0 or close to 0% MS) in top 8 brands
     # cumulatively, every year more than 95.1% market share in Volume and 92.4% in Value
   
   BSB.keep_DIRK <- subbrands_MS_per_retailer$DIRK$BSBRetailer[1:8]
   TP_purch$BSBRetailer[TP_purch$Banner_name=="DIRK"] <- ifelse(TP_purch$BSBRetailer[TP_purch$Banner_name=="DIRK"] %in% BSB.keep_DIRK,
                                                                TP_purch$BSBRetailer[TP_purch$Banner_name=="DIRK"], "DIRK RestBSB")
   
   
   #Jumbo
   subbrands_MS_per_retailer$Jumbo <- subbrands_MS_per_retailer$Jumbo[order(subbrands_MS_per_retailer$Jumbo$MS_Value.overall, decreasing=T),]
   subbrands_MS_per_retailer$Jumbo
   
     # cut off at 4 sub-brands
     # every year at least 2.4% in Value and 2.6% MS in Volume
     # cumulatively, every year more than 88.5% MS in Volume and 87.9% in Value
   
   BSB.keep_Jumbo <- subbrands_MS_per_retailer$Jumbo$BSBRetailer[1:4]
   TP_purch$BSBRetailer[TP_purch$Banner_name=="Jumbo"] <- ifelse(TP_purch$BSBRetailer[TP_purch$Banner_name=="Jumbo"] %in% BSB.keep_Jumbo,
                                                                 TP_purch$BSBRetailer[TP_purch$Banner_name=="Jumbo"], "Jumbo RestBSB")
   
   
   
   #Lidl
   subbrands_MS_per_retailer$Lidl <- subbrands_MS_per_retailer$Lidl[order(subbrands_MS_per_retailer$Lidl$MS_Value.overall, decreasing = T),]
   subbrands_MS_per_retailer$Lidl   

     # only 3 subbrands, of which one has 99+% market share
   
   BSB.keep_Lidl <- subbrands_MS_per_retailer$Lidl$BSBRetailer[1]
   TP_purch$BSBRetailer[TP_purch$Banner_name == "Lidl"] <- ifelse(TP_purch$BSBRetailer[TP_purch$Banner_name=="Lidl"] %in% BSB.keep_Lidl,
                                                                  TP_purch$BSBRetailer[TP_purch$Banner_name=="Lidl"], "Lidl RestBSB")
   
   
   #Plus
   subbrands_MS_per_retailer$Plus <- subbrands_MS_per_retailer$Plus[order(subbrands_MS_per_retailer$Plus$MS_Value.overall, decreasing = T),]
   subbrands_MS_per_retailer$Plus
   
     # cut off at 9 sub-brands
     # a lot of listings and delistings
     # cumulatively, every year more than 96.1% MS in Volume and 97.3% in Value
   
   BSB.keep_Plus <- subbrands_MS_per_retailer$Plus$BSBRetailer[1:9]
   TP_purch$BSBRetailer[TP_purch$Banner_name=="Plus"] <- ifelse(TP_purch$BSBRetailer[TP_purch$Banner_name=="Plus"] %in% BSB.keep_Plus,
                                                                TP_purch$BSBRetailer[TP_purch$Banner_name=="Plus"], "Plus RestBSB")
   
   
   #Rest Retailer
   subbrands_MS_per_retailer$RestRetailer <- subbrands_MS_per_retailer$RestRetailer[order(subbrands_MS_per_retailer$RestRetailer$MS_Value.overall, decreasing=T),]
   subbrands_MS_per_retailer$RestRetailer
   
     # cut off at 8 sub-brands
     # every year at least 2.1% MS in Volume and 2.6% in Value
     # cumulatively, every year more than 71.7% MS in Volume and 71.8% in Value
   
   BSB.keep_rest <- subbrands_MS_per_retailer$RestRetailer$BSBRetailer[1:8]
   TP_purch$BSBRetailer[TP_purch$Banner_name=="RestRetailer"] <- ifelse(TP_purch$BSBRetailer[TP_purch$Banner_name=="RestRetailer"] %in% BSB.keep_rest,
                                                                        TP_purch$BSBRetailer[TP_purch$Banner_name=="RestRetailer"], "RestRetailer RestBSB")

   
rm(list=c("subbrands_MS","subbrands_MS_across_years","subbrands_MS_per_retailer"))
BSB.keep = c(BSB.keep_AH, BSB.keep_Aldi, BSB.keep_DIRK, BSB.keep_Jumbo, BSB.keep_Lidl, BSB.keep_Plus, BSB.keep_rest)
rm(list= c("BSB.keep_AH","BSB.keep_Aldi","BSB.keep_DIRK","BSB.keep_Jumbo","BSB.keep_Lidl","BSB.keep_Plus","BSB.keep_rest"))

BSB_retailer <- unique(TP_purch$BSBRetailer)




####################################################################################################################
# Imputing prices: 95th percentile of year < 95th percentile of quartile < 95th percentile of month < average of day
####################################################################################################################

# quarter variable already included in data
table(TP_purch$Quarter)

# create month variable
library(lubridate)
TP_purch$Month <- paste(TP_purch$Year, month(as.POSIXlt(TP_purch$Date_of_purchase)))
TP_purch$Month <- factor(TP_purch$Month)

# calculate price PAID per roll (per transaction)
TP_purch$Price <- TP_purch$Total_value_sales / TP_purch$Total_volume_sales / 100

# 95th percentile of Year
prices_yearly <- ddply(TP_purch, .(Year, BSBRetailer), summarise, price_per_roll_year=quantile(Price, probs=(0.95)), Obs_y = length(Date_of_purchase))

# 95th percentile of Quarter
prices_quarterly <- ddply(TP_purch,.(Quarter,BSBRetailer),summarise,price_per_roll_quarter=quantile(Price,probs=(0.95)), Obs_q = length(Date_of_purchase))

#95th percentile of price per Month
prices_monthly <- ddply(TP_purch,.(Month,BSBRetailer),summarise,price_per_roll_month=quantile(Price,probs=(0.95)), Obs_m = length(Date_of_purchase))

# average price of Day
prices_daily <- ddply(TP_purch,.(Date_of_purchase,BSBRetailer),summarise,price_per_roll_day=mean(Price), percentagePromo = mean(Promo), Obs_d = length(Date_of_purchase))











# Huang, Y., & Bronnenberg, B. J. (2018). Pennies for your thoughts: Costly product consideration and purchase quantity thresholds. Marketing Science, 37(6), 1009-1028.
# regular price = 95th percentile of price in given store-year










#### CHECK WHETHER THERE IS A PURCHASE ON EACH OF 852 DAYS ###
# create all dates (with Year, Quarter and Month)
dates <- ddply(TP_purch, .(Date_of_purchase,Year,Quarter,Month),summarise)
length(dates$Date_of_purchase) == 852
# combine BrandSubBrand-retailer with dates
PricePromoIndex <-  do.call("rbind", replicate(length(BSB_retailer), dates, simplify = FALSE))
PricePromoIndex$BSBRetailer <- rep(BSB_retailer, each=length(observation.dates))

# match with 95th percentile of price per YEAR
PricePromoIndex <- merge(PricePromoIndex, prices_yearly, by=c("Year", "BSBRetailer"), all.x = T)
# match with 95th percentile of price per QUARTER
PricePromoIndex <- merge(PricePromoIndex, prices_quarterly, by=c("Quarter", "BSBRetailer"), all.x=T)
# match with 95th percentile of price per MONTH
PricePromoIndex <- merge(PricePromoIndex, prices_monthly, by=c("Month","BSBRetailer"), all.x=T)
# match with average price per DAY
PricePromoIndex <- merge(PricePromoIndex, prices_daily, by=c("Date_of_purchase","BSBRetailer"), all.x=T)

PricePromoIndex$ShelfPriceIndex <- PricePromoIndex$price_per_roll_day
PricePromoIndex$RegularPriceIndex <- PricePromoIndex$price_per_roll_year
PricePromoIndex$RegularPriceIndex_Q <- PricePromoIndex$price_per_roll_quarter
# if ShelfPriceIndex is NA: take price_per_roll_month
# if then still NA: take price_per_roll_quarter
# if then still NA: take price_per_roll_year
for (i in seq_along(PricePromoIndex$Date_of_purchase)) {
  PricePromoIndex$ShelfPriceIndex[i] <- ifelse(is.na(PricePromoIndex$ShelfPriceIndex[i]), PricePromoIndex$price_per_roll_month[i], PricePromoIndex$ShelfPriceIndex[i])
  PricePromoIndex$ShelfPriceIndex[i] <- ifelse(is.na(PricePromoIndex$ShelfPriceIndex[i]), PricePromoIndex$price_per_roll_quarter[i], PricePromoIndex$ShelfPriceIndex[i])
  PricePromoIndex$ShelfPriceIndex[i] <- ifelse(is.na(PricePromoIndex$ShelfPriceIndex[i]), PricePromoIndex$price_per_roll_year[i], PricePromoIndex$ShelfPriceIndex[i])
}


# # STILL MISSING VALUES!! this means sometimes a BrandSubBrand is not bought at a retailer for a whole year
# test <- ddply(PricePromoIndex, .(BSBRetailer, Quarter), summarise, Qprice=mean(price_per_roll_quarter), YPrice= mean(price_per_roll_year), PIndex=mean(ShelfPriceIndex))
# test <- test %>% group_by(BSBRetailer) %>% summarise(sumNA = sum(is.na(PIndex)))
# table(test$sumNA)
# # probably listings and delistings -> will be taken into account in weights (reweighting based on availability)


rm(list=c('prices_daily','prices_monthly','prices_quarterly', 'prices_yearly'))






####################################################################
# imputing Promo: % of purchases on promo per day -> cut off at 0.5
####################################################################

# sort data by BSBRetailer, then Date
PricePromoIndex <- PricePromoIndex[order(PricePromoIndex$BSBRetailer, PricePromoIndex$Date_of_purchase), ]
# replace NAs in PercentagePromo with 0 (otherwise ifelse condition doesn't work)
# this boils down to treating days without a purchase like a day with only non-promo purchases
PricePromoIndex$percentagePromo[is.na(PricePromoIndex$percentagePromo)] <- 0
# create Promo Index
# if more than 50% of purchases that day are on promo => PROMO DAY
PricePromoIndex$PromoIndex <- ifelse(PricePromoIndex$percentagePromo > 0.5, 1, NA)

# first and last day of observation period will be special cases
special.dates <- c(as.Date("2018-01-01"), as.Date("2020-05-01"))
# one loop through whole dataframe, make extra condition for 1/1/2018 and 31/12/2020
for (i in seq_along(PricePromoIndex$Date_of_purchase)) {
  if (!PricePromoIndex$Date_of_purchase[i] %in% special.dates) {
    # if percentagePromo <= 0.5% (so less than 50% of purchases are in promo OR no purchase was made that day)
    # if day before and day after are promo => PROMO DAY
    PricePromoIndex$PromoIndex[i] <- ifelse(PricePromoIndex$percentagePromo[i] <= 0.5 & PricePromoIndex$percentagePromo[i-1]>0.5 & PricePromoIndex$percentagePromo[i+1]>0.5, 1, PricePromoIndex$PromoIndex[i])
    # if day before and/or day after are not promo => NON PROMO DAY
    PricePromoIndex$PromoIndex[i] <- ifelse(PricePromoIndex$percentagePromo[i] <= 0.5 & (PricePromoIndex$percentagePromo[i-1]<=0.5 | PricePromoIndex$percentagePromo[i+1]<=0.5), 0, PricePromoIndex$PromoIndex[i])
  } else if (PricePromoIndex$Date_of_purchase[i] == "2018-01-01") {
    ### 1 JANUARY 2018 ###
    # if day after is promo => PROMO DAY
    PricePromoIndex$PromoIndex[i] <- ifelse(PricePromoIndex$percentagePromo[i] <= 0.5 & PricePromoIndex$percentagePromo[i+1]>0.5, 1, PricePromoIndex$PromoIndex[i])
    # if day before and/or day after are not promo => NON PROMO DAY
    PricePromoIndex$PromoIndex[i] <- ifelse(PricePromoIndex$percentagePromo[i] <= 0.5 & PricePromoIndex$percentagePromo[i+1]<=0.5, 0, PricePromoIndex$PromoIndex[i])
  } else if (PricePromoIndex$Date_of_purchase[i] == "2020-05-01") {
    # if day after is promo => PROMO DAY
    PricePromoIndex$PromoIndex[i] <- ifelse(PricePromoIndex$percentagePromo[i] <= 0.5 & PricePromoIndex$percentagePromo[i-1]>0.5, 1, PricePromoIndex$PromoIndex[i])
    # if day before and/or day after are not promo => NON PROMO DAY
    PricePromoIndex$PromoIndex[i] <- ifelse(PricePromoIndex$percentagePromo[i] <= 0.5 & PricePromoIndex$percentagePromo[i-1]<=0.5, 0, PricePromoIndex$PromoIndex[i])
  }
}
## note: whether day before/after are promo days is inferred from percentagepromo, because PromoIndex is in most cases NA so ifelse condition is never fulfilled!


# how often promo per BSBRetailer?
temp <- PricePromoIndex %>% group_by(BSBRetailer) %>% summarise(promo = sum(PromoIndex == 1))
temp$percent <- round(temp$promo / 852, 4)
rm(temp)




# overrule shelf price index if PROMO - NON-PROMO - PROMO with average of price of 2 promo days
for (i in seq_along(PricePromoIndex$Date_of_purchase)) {
  if (! PricePromoIndex$Date_of_purchase[i] %in% special.dates) {
    PricePromoIndex$ShelfPriceIndex[i] <- ifelse(PricePromoIndex$percentagePromo[i] <= 0.5 & PricePromoIndex$percentagePromo[i-1]>0.5 & PricePromoIndex$percentagePromo[i+1]>0.5, 
                                            mean(c(PricePromoIndex$ShelfPriceIndex[i-1], PricePromoIndex$ShelfPriceIndex[i+1])),
                                            PricePromoIndex$ShelfPriceIndex[i])
  }}


# check price time series
# plots <- PricePromoIndex %>% group_by(BSBRetailer) %>% group_map(~ggplot(data=.) + aes(x=Date_of_purchase, y=PriceIndex) + geom_line())
# rm(plots)








######################################################
# create household weights for price and promo indices
######################################################

all.households <- unique(TP_purch$Panelist)
HHweights <- data.frame(Panelist = rep(all.households, each=length(BSB_retailer)), BSBRetailer =rep(BSB_retailer, times=length(all.households)))

# calculate total volume bought per household of each BSBRetailer
temp <- ddply(TP_purch, .(Panelist, BSBRetailer), summarise, Vol=sum(Total_volume_sales))
HHweights <- merge(HHweights, temp, all.x=T, by=c("Panelist","BSBRetailer"))
rm(temp)

# if not bought -> NA. Better to replace with zeros to calculate sums etc.
HHweights$Vol[is.na(HHweights$Vol)] <- 0
# calculate total volume bought per household (across entire observation period)
HHweights <- HHweights %>% group_by(Panelist) %>% mutate(TotHH_Whole = sum(Vol, na.rm = T))

# market share weights calculated based on whole observation period
HHweights$Weights <- HHweights$Vol / HHweights$TotHH_Whole


# check that for every household, the weights sum to 1
test <- HHweights %>% group_by(Panelist) %>% summarise(sumweights = sum(Weights))
table(test$sumweights)
rm(test)



BSB_firstlast <- TP_purch %>% group_by(BSBRetailer) %>% arrange(Date_of_purchase) %>% mutate(firstQ = Quarter[row_number() == 1], lastQ=Quarter[row_number() == n()] ) %>% select("BSBRetailer", "firstQ", "lastQ") 
BSB_firstlast <- unique(BSB_firstlast)
BSB_firstlast$lastQ[BSB_firstlast$lastQ == "202001"] <- "202002"
all.quarters <- unique(TP_purch$Quarter)
quartercheck <- data.frame(BSBRetailer = rep(BSB_retailer, each=length(all.quarters)), Quarter = rep(all.quarters, times=length(BSB_retailer)))
quartercheck <- merge(quartercheck, BSB_firstlast, by="BSBRetailer", all.x=T)
quartercheck[,c("Quarter","firstQ","lastQ")] <- format(quartercheck[,c("Quarter","firstQ","lastQ")], format="%y%q")
quartercheck <- quartercheck[order(quartercheck$BSBRetailer, quartercheck$Quarter), ]
quartercheck$available <- ifelse(quartercheck$Quarter <= quartercheck$lastQ & quartercheck$Quarter >= quartercheck$firstQ, T, F)
quartercheck <- quartercheck[, c("BSBRetailer", "Quarter","available")]

# create column per quarter with 0-1 whether available or not
quartercheck <- reshape(as.data.frame(quartercheck), timevar = "Quarter", idvar = "BSBRetailer", direction = "wide")
# match the dimensions of HHweights
unique(quartercheck$BSBRetailer) == HHweights$BSBRetailer[1:43]

# add "availability" checks to HHweights
HHweights <- merge(HHweights, quartercheck, by="BSBRetailer", all.x=T)

# rescale the weights based on which products are avaiable in the quarter
HHweights$Weights_201801 <- HHweights$Weights * HHweights$available.201801
HHweights$Weights_201802 <- HHweights$Weights * HHweights$available.201802
HHweights$Weights_201803 <- HHweights$Weights * HHweights$available.201803
HHweights$Weights_201804 <- HHweights$Weights * HHweights$available.201804
HHweights$Weights_201901 <- HHweights$Weights * HHweights$available.201901
HHweights$Weights_201902 <- HHweights$Weights * HHweights$available.201902
HHweights$Weights_201903 <- HHweights$Weights * HHweights$available.201903
HHweights$Weights_201904 <- HHweights$Weights * HHweights$available.201904
HHweights$Weights_202001 <- HHweights$Weights * HHweights$available.202001
HHweights$Weights_202002 <- HHweights$Weights * HHweights$available.202002
HHweights <- HHweights %>% group_by(Panelist) %>% mutate(Weights_201801 = Weights_201801 / sum(Weights_201801), Weights_201802 = Weights_201802 / sum(Weights_201802),
                                                         Weights_201803 = Weights_201803 / sum(Weights_201803), Weights_201804 = Weights_201804 / sum(Weights_201804),
                                                         Weights_201901 = Weights_201901 / sum(Weights_201901), Weights_201902 = Weights_201902 / sum(Weights_201902),
                                                         Weights_201903 = Weights_201903 / sum(Weights_201903), Weights_201904 = Weights_201904 / sum(Weights_201904),
                                                         Weights_202001 = Weights_202001 / sum(Weights_202001), Weights_202002 = Weights_202002 / sum(Weights_202002))
# remove redundant columns
HHweights <- HHweights[, c("Panelist","BSBRetailer","Weights_201801","Weights_201802","Weights_201803","Weights_201804",
                           "Weights_201901","Weights_201902","Weights_201903","Weights_201904","Weights_202001","Weights_202002")]
rm(list = c('BSB_firstlast', 'quartercheck'))







#########################################
# create weighted PRICE and PROMO indices
#########################################

#temp save HHweights
save.HHweights <- HHweights

# make sure the order of BSBRetailer is the same in PricePromoIndex and HHWeights
table(unique(PricePromoIndex$BSBRetailer) == unique(HHweights$BSBRetailer))

# turn HHWeights into list
HHweights <- dlply(HHweights, .(Panelist), c)
PricePromoIndex <- PricePromoIndex[order(PricePromoIndex$Date_of_purchase),c("Date_of_purchase", "BSBRetailer","PromoIndex", "ShelfPriceIndex", "RegularPriceIndex", "RegularPriceIndex_Q")]

# the weights of not-yet-introduced or delisted products are zero
# so we can impute prices with anything to get rid of NAs -> will not translate into price index
PricePromoIndex$ShelfPriceIndex[is.na(PricePromoIndex$ShelfPriceIndex)] <- 0
PricePromoIndex$RegularPriceIndex[is.na(PricePromoIndex$RegularPriceIndex)] <- 0
PricePromoIndex$RegularPriceIndex_Q[is.na(PricePromoIndex$RegularPriceIndex_Q)] <- 0

# split PricePromoIndex per quarter
PricePromoIndex_201801 <- PricePromoIndex[sub(pattern = " Q", replacement = "0", x= as.yearqtr(PricePromoIndex$Date_of_purchase)) == "201801",]
PricePromoIndex_201802 <- PricePromoIndex[sub(pattern = " Q", replacement = "0", x= as.yearqtr(PricePromoIndex$Date_of_purchase)) == "201802",]
PricePromoIndex_201803 <- PricePromoIndex[sub(pattern = " Q", replacement = "0", x= as.yearqtr(PricePromoIndex$Date_of_purchase)) == "201803",]
PricePromoIndex_201804 <- PricePromoIndex[sub(pattern = " Q", replacement = "0", x= as.yearqtr(PricePromoIndex$Date_of_purchase)) == "201804",]
PricePromoIndex_201901 <- PricePromoIndex[sub(pattern = " Q", replacement = "0", x= as.yearqtr(PricePromoIndex$Date_of_purchase)) == "201901",]
PricePromoIndex_201902 <- PricePromoIndex[sub(pattern = " Q", replacement = "0", x= as.yearqtr(PricePromoIndex$Date_of_purchase)) == "201902",]
PricePromoIndex_201903 <- PricePromoIndex[sub(pattern = " Q", replacement = "0", x= as.yearqtr(PricePromoIndex$Date_of_purchase)) == "201903",]
PricePromoIndex_201904 <- PricePromoIndex[sub(pattern = " Q", replacement = "0", x= as.yearqtr(PricePromoIndex$Date_of_purchase)) == "201904",]
PricePromoIndex_202001 <- PricePromoIndex[sub(pattern = " Q", replacement = "0", x= as.yearqtr(PricePromoIndex$Date_of_purchase)) == "202001",]
PricePromoIndex_202002 <- PricePromoIndex[sub(pattern = " Q", replacement = "0", x= as.yearqtr(PricePromoIndex$Date_of_purchase)) == "202002",]

# make Household-specific weighted price and promo index per day
final_PricePromoIndex <- data.frame(Date = rep(observation.dates, times=length(all.households)), Household = rep(unique(save.HHweights$Panelist), each=length(observation.dates)))
for (i in seq_along(all.households)) {
  HHweights[[i]]$ShelfPriceIndex201801 <- PricePromoIndex_201801$ShelfPriceIndex * HHweights[[i]]$Weights_201801
  HHweights[[i]]$ShelfPriceIndex201801 <- colSums(matrix(HHweights[[i]]$ShelfPriceIndex201801, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex201801 <- PricePromoIndex_201801$RegularPriceIndex * HHweights[[i]]$Weights_201801
  HHweights[[i]]$RegularPriceIndex201801 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex201801, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex_Q201801 <- PricePromoIndex_201801$RegularPriceIndex_Q * HHweights[[i]]$Weights_201801
  HHweights[[i]]$RegularPriceIndex_Q201801 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex_Q201801, nrow=length(BSB_retailer)))
  HHweights[[i]]$PromoIndex201801 <- PricePromoIndex_201801$PromoIndex * HHweights[[i]]$Weights_201801
  HHweights[[i]]$PromoIndex201801 <- colSums(matrix(HHweights[[i]]$PromoIndex201801, nrow=length(BSB_retailer)))

  HHweights[[i]]$ShelfPriceIndex201802 <- PricePromoIndex_201802$ShelfPriceIndex * HHweights[[i]]$Weights_201802
  HHweights[[i]]$ShelfPriceIndex201802 <- colSums(matrix(HHweights[[i]]$ShelfPriceIndex201802, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex201802 <- PricePromoIndex_201802$RegularPriceIndex * HHweights[[i]]$Weights_201802
  HHweights[[i]]$RegularPriceIndex201802 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex201802, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex_Q201802 <- PricePromoIndex_201802$RegularPriceIndex_Q * HHweights[[i]]$Weights_201802
  HHweights[[i]]$RegularPriceIndex_Q201802 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex_Q201802, nrow=length(BSB_retailer)))
  HHweights[[i]]$PromoIndex201802 <- PricePromoIndex_201802$PromoIndex * HHweights[[i]]$Weights_201802
  HHweights[[i]]$PromoIndex201802 <- colSums(matrix(HHweights[[i]]$PromoIndex201802, nrow=length(BSB_retailer)))
  
  HHweights[[i]]$ShelfPriceIndex201803 <- PricePromoIndex_201803$ShelfPriceIndex * HHweights[[i]]$Weights_201803
  HHweights[[i]]$ShelfPriceIndex201803 <- colSums(matrix(HHweights[[i]]$ShelfPriceIndex201803, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex201803 <- PricePromoIndex_201803$RegularPriceIndex * HHweights[[i]]$Weights_201803
  HHweights[[i]]$RegularPriceIndex201803 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex201803, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex_Q201803 <- PricePromoIndex_201803$RegularPriceIndex_Q * HHweights[[i]]$Weights_201803
  HHweights[[i]]$RegularPriceIndex_Q201803 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex_Q201803, nrow=length(BSB_retailer)))
  HHweights[[i]]$PromoIndex201803 <- PricePromoIndex_201803$PromoIndex * HHweights[[i]]$Weights_201803
  HHweights[[i]]$PromoIndex201803 <- colSums(matrix(HHweights[[i]]$PromoIndex201803, nrow=length(BSB_retailer)))
  
  HHweights[[i]]$ShelfPriceIndex201804 <- PricePromoIndex_201804$ShelfPriceIndex * HHweights[[i]]$Weights_201804
  HHweights[[i]]$ShelfPriceIndex201804 <- colSums(matrix(HHweights[[i]]$ShelfPriceIndex201804, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex201804 <- PricePromoIndex_201804$RegularPriceIndex * HHweights[[i]]$Weights_201804
  HHweights[[i]]$RegularPriceIndex201804 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex201804, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex_Q201804 <- PricePromoIndex_201804$RegularPriceIndex_Q * HHweights[[i]]$Weights_201804
  HHweights[[i]]$RegularPriceIndex_Q201804 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex_Q201804, nrow=length(BSB_retailer)))
  HHweights[[i]]$PromoIndex201804 <- PricePromoIndex_201804$PromoIndex * HHweights[[i]]$Weights_201804
  HHweights[[i]]$PromoIndex201804 <- colSums(matrix(HHweights[[i]]$PromoIndex201804, nrow=length(BSB_retailer)))
  
  HHweights[[i]]$ShelfPriceIndex201901 <- PricePromoIndex_201901$ShelfPriceIndex * HHweights[[i]]$Weights_201901
  HHweights[[i]]$ShelfPriceIndex201901 <- colSums(matrix(HHweights[[i]]$ShelfPriceIndex201901, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex201901 <- PricePromoIndex_201901$RegularPriceIndex * HHweights[[i]]$Weights_201901
  HHweights[[i]]$RegularPriceIndex201901 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex201901, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex_Q201901 <- PricePromoIndex_201901$RegularPriceIndex_Q * HHweights[[i]]$Weights_201901
  HHweights[[i]]$RegularPriceIndex_Q201901 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex_Q201901, nrow=length(BSB_retailer)))
  HHweights[[i]]$PromoIndex201901 <- PricePromoIndex_201901$PromoIndex * HHweights[[i]]$Weights_201901
  HHweights[[i]]$PromoIndex201901 <- colSums(matrix(HHweights[[i]]$PromoIndex201901, nrow=length(BSB_retailer)))
  
  HHweights[[i]]$ShelfPriceIndex201902 <- PricePromoIndex_201902$ShelfPriceIndex * HHweights[[i]]$Weights_201902
  HHweights[[i]]$ShelfPriceIndex201902 <- colSums(matrix(HHweights[[i]]$ShelfPriceIndex201902, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex201902 <- PricePromoIndex_201902$RegularPriceIndex * HHweights[[i]]$Weights_201902
  HHweights[[i]]$RegularPriceIndex201902 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex201902, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex_Q201902 <- PricePromoIndex_201902$RegularPriceIndex_Q * HHweights[[i]]$Weights_201902
  HHweights[[i]]$RegularPriceIndex_Q201902 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex_Q201902, nrow=length(BSB_retailer)))
  HHweights[[i]]$PromoIndex201902 <- PricePromoIndex_201902$PromoIndex * HHweights[[i]]$Weights_201902
  HHweights[[i]]$PromoIndex201902 <- colSums(matrix(HHweights[[i]]$PromoIndex201902, nrow=length(BSB_retailer)))
  
  HHweights[[i]]$ShelfPriceIndex201903 <- PricePromoIndex_201903$ShelfPriceIndex * HHweights[[i]]$Weights_201903
  HHweights[[i]]$ShelfPriceIndex201903 <- colSums(matrix(HHweights[[i]]$ShelfPriceIndex201903, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex201903 <- PricePromoIndex_201903$RegularPriceIndex * HHweights[[i]]$Weights_201903
  HHweights[[i]]$RegularPriceIndex201903 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex201903, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex_Q201903 <- PricePromoIndex_201903$RegularPriceIndex_Q * HHweights[[i]]$Weights_201903
  HHweights[[i]]$RegularPriceIndex_Q201903 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex_Q201903, nrow=length(BSB_retailer)))
  HHweights[[i]]$PromoIndex201903 <- PricePromoIndex_201903$PromoIndex * HHweights[[i]]$Weights_201903
  HHweights[[i]]$PromoIndex201903 <- colSums(matrix(HHweights[[i]]$PromoIndex201903, nrow=length(BSB_retailer)))
  
  HHweights[[i]]$ShelfPriceIndex201904 <- PricePromoIndex_201904$ShelfPriceIndex * HHweights[[i]]$Weights_201904
  HHweights[[i]]$ShelfPriceIndex201904 <- colSums(matrix(HHweights[[i]]$ShelfPriceIndex201904, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex201904 <- PricePromoIndex_201904$RegularPriceIndex * HHweights[[i]]$Weights_201904
  HHweights[[i]]$RegularPriceIndex201904 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex201904, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex_Q201904 <- PricePromoIndex_201904$RegularPriceIndex_Q * HHweights[[i]]$Weights_201904
  HHweights[[i]]$RegularPriceIndex_Q201904 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex_Q201904, nrow=length(BSB_retailer)))
  HHweights[[i]]$PromoIndex201904 <- PricePromoIndex_201904$PromoIndex * HHweights[[i]]$Weights_201904
  HHweights[[i]]$PromoIndex201904 <- colSums(matrix(HHweights[[i]]$PromoIndex201904, nrow=length(BSB_retailer)))
  
  HHweights[[i]]$ShelfPriceIndex202001 <- PricePromoIndex_202001$ShelfPriceIndex * HHweights[[i]]$Weights_202001
  HHweights[[i]]$ShelfPriceIndex202001 <- colSums(matrix(HHweights[[i]]$ShelfPriceIndex202001, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex202001 <- PricePromoIndex_202001$RegularPriceIndex * HHweights[[i]]$Weights_202001
  HHweights[[i]]$RegularPriceIndex202001 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex202001, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex_Q202001 <- PricePromoIndex_202001$RegularPriceIndex_Q * HHweights[[i]]$Weights_202001
  HHweights[[i]]$RegularPriceIndex_Q202001 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex_Q202001, nrow=length(BSB_retailer)))
  HHweights[[i]]$PromoIndex202001 <- PricePromoIndex_202001$PromoIndex * HHweights[[i]]$Weights_202001
  HHweights[[i]]$PromoIndex202001 <- colSums(matrix(HHweights[[i]]$PromoIndex202001, nrow=length(BSB_retailer)))
  
  HHweights[[i]]$ShelfPriceIndex202002 <- PricePromoIndex_202002$ShelfPriceIndex * HHweights[[i]]$Weights_202002
  HHweights[[i]]$ShelfPriceIndex202002 <- colSums(matrix(HHweights[[i]]$ShelfPriceIndex202002, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex202002 <- PricePromoIndex_202002$RegularPriceIndex * HHweights[[i]]$Weights_202002
  HHweights[[i]]$RegularPriceIndex202002 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex202002, nrow=length(BSB_retailer)))
  HHweights[[i]]$RegularPriceIndex_Q202002 <- PricePromoIndex_202002$RegularPriceIndex_Q * HHweights[[i]]$Weights_202002
  HHweights[[i]]$RegularPriceIndex_Q202002 <- colSums(matrix(HHweights[[i]]$RegularPriceIndex_Q202002, nrow=length(BSB_retailer)))
  HHweights[[i]]$PromoIndex202002 <- PricePromoIndex_202002$PromoIndex * HHweights[[i]]$Weights_202002
  HHweights[[i]]$PromoIndex202002 <- colSums(matrix(HHweights[[i]]$PromoIndex202002, nrow=length(BSB_retailer)))
  
    # make Price and Promo index for a household one vector (ordered over time)
  HHweights[[i]]$ShelfPriceIndex <- c(HHweights[[i]]$ShelfPriceIndex201801, HHweights[[i]]$ShelfPriceIndex201802, HHweights[[i]]$ShelfPriceIndex201803,
                                 HHweights[[i]]$ShelfPriceIndex201804, HHweights[[i]]$ShelfPriceIndex201901, HHweights[[i]]$ShelfPriceIndex201902,
                                 HHweights[[i]]$ShelfPriceIndex201903, HHweights[[i]]$ShelfPriceIndex201904, HHweights[[i]]$ShelfPriceIndex202001,
                                 HHweights[[i]]$ShelfPriceIndex202002)
  HHweights[[i]]$RegularPriceIndex <- c(HHweights[[i]]$RegularPriceIndex201801, HHweights[[i]]$RegularPriceIndex201802, HHweights[[i]]$RegularPriceIndex201803,
                                 HHweights[[i]]$RegularPriceIndex201804, HHweights[[i]]$RegularPriceIndex201901, HHweights[[i]]$RegularPriceIndex201902,
                                 HHweights[[i]]$RegularPriceIndex201903, HHweights[[i]]$RegularPriceIndex201904, HHweights[[i]]$RegularPriceIndex202001,
                                 HHweights[[i]]$RegularPriceIndex202002)
  HHweights[[i]]$RegularPriceIndex_Q <- c(HHweights[[i]]$RegularPriceIndex_Q201801, HHweights[[i]]$RegularPriceIndex_Q201802, HHweights[[i]]$RegularPriceIndex_Q201803,
                                          HHweights[[i]]$RegularPriceIndex_Q201804, HHweights[[i]]$RegularPriceIndex_Q201901, HHweights[[i]]$RegularPriceIndex_Q201902,
                                          HHweights[[i]]$RegularPriceIndex_Q201903, HHweights[[i]]$RegularPriceIndex_Q201904, HHweights[[i]]$RegularPriceIndex_Q202001,
                                          HHweights[[i]]$RegularPriceIndex_Q202002)
  HHweights[[i]]$PromoIndex <- c(HHweights[[i]]$PromoIndex201801, HHweights[[i]]$PromoIndex201802, HHweights[[i]]$PromoIndex201803,
                                 HHweights[[i]]$PromoIndex201804, HHweights[[i]]$PromoIndex201901, HHweights[[i]]$PromoIndex201902,
                                 HHweights[[i]]$PromoIndex201903, HHweights[[i]]$PromoIndex201904, HHweights[[i]]$PromoIndex202001,
                                 HHweights[[i]]$PromoIndex202002)
  # paste together all households' Price and Promo indices
  if (i == 1) { ShelfPriceIndex <- HHweights[[i]]$ShelfPriceIndex
                RegularPriceIndex <- HHweights[[i]]$RegularPriceIndex
                RegularPriceIndex_Q <- HHweights[[i]]$RegularPriceIndex_Q
                PromoIndex <- HHweights[[i]]$PromoIndex
  } else {ShelfPriceIndex <- c(ShelfPriceIndex, HHweights[[i]]$ShelfPriceIndex)
          RegularPriceIndex <- c(RegularPriceIndex, HHweights[[i]]$RegularPriceIndex)
          RegularPriceIndex_Q <- c(RegularPriceIndex_Q, HHweights[[i]]$RegularPriceIndex_Q)
          PromoIndex <- c(PromoIndex, HHweights[[i]]$PromoIndex) } 
}

final_PricePromoIndex$ShelfPriceIndex <- ShelfPriceIndex
final_PricePromoIndex$RegularPriceIndex <- RegularPriceIndex
final_PricePromoIndex$RegularPriceIndex_Q <- RegularPriceIndex_Q
final_PricePromoIndex$PromoIndex <- PromoIndex


rm(list=c('PricePromoIndex_201801', 'PricePromoIndex_201802', 'PricePromoIndex_201803', 'PricePromoIndex_201804',
          'PricePromoIndex_201901', 'PricePromoIndex_201902', 'PricePromoIndex_201903', 'PricePromoIndex_201904',
          'PricePromoIndex_202001', 'PricePromoIndex_202002', 'PricePromoIndex', 'save.HHweights', 'ShelfPriceIndex', 'RegularPriceIndex', 'RegularPriceIndex_Q'))





################################
# save price and promotion index
################################

save(final_PricePromoIndex, file=paste(project_path,"/R_files/INC-QUANT model/toilet paper/PricePromoIndex_NL.RData",sep=""))


save(TP_purch, file=paste(project_path,"/R_files/INC-QUANT model/toilet paper/prepared_purchasedata_NL",sep=""))



