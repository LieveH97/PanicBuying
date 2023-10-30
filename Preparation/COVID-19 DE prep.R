##############################################
###                                        ###
###    Preparing DE COVID Health Figures   ###
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



#load libraries - you have to install the packages before running the code!
list.of.packages <- c("plyr")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)


####################
# Loading data
####################

#raw dataset
raw <- read.csv("./Data/covid_data/DE RKI_28.04.2021.csv", sep=",")

#dataframe to manipulate
DEdat <- data.frame(raw)



#######################
# Inspecting data
#######################

str(DEdat)
summary(DEdat)

#Meldedatum is date of covid case
DEdat$Meldedatum <- gsub("00:00:00+00","",DEdat$Meldedatum, fixed = T)
DEdat$Meldedatum <- as.Date(DEdat$Meldedatum, format="%Y/%m/%d")
DEdat$Bundesland <- factor(DEdat$Bundesland)
levels(DEdat$Bundesland)

#translate necessary variables to English
DEdat <- rename(DEdat, replace = c("Meldedatum"="Date", "Bundesland"="Province","AnzahlFall"="NewCases", "AnzahlTodesfall"="NewDeaths"))
str(DEdat)



############################################
# CREATING REGIONAL AND NATIONAL DATASETS
############################################

# creating regional dataset
DEdat_reg <- ddply(DEdat,.(Date,Province),summarise,
               DailyCases=sum(na.omit(NewCases)),DailyDeaths=sum(na.omit(NewDeaths)))
DE_provinces <- data.frame(region_id=1:length(unique(DEdat$Province)),province_name=levels(DEdat$Province))

# creating national dataset
DEdat_nat <- ddply(DEdat,.(Date),summarise,DailyCases=sum(na.omit(NewCases)),DailyDeaths=sum(na.omit(NewDeaths)))

rm(DEdat)




##############################################################################
#                             REGIONAL DATASET
##############################################################################

### INCLUDING NON-OBSERVATIONS ###

#now, only observations for dates where a case or death was reported in the province
all.dates <- seq(as.Date("2020-01-07",format="%Y-%m-%d"), as.Date("2021-04-27",format="%Y-%m-%d"), by="days")
#loop over provinces -> create new rows with missing date + province name
for (i in seq_along(DE_provinces$province_name)) {
  miss.dates <- subset(all.dates, !(all.dates %in% DEdat_reg$Date[DEdat_reg$Province==DE_provinces$province_name[i]]))
  miss.dates <- as.Date(miss.dates,format="%Y-%m-%d")
  add <- cbind(as.character(miss.dates), as.character(DE_provinces$province_name[i]), 0, 0)
  colnames(add) <- c("Date", "Province","DailyCases", "DailyDeaths")
  DEdat_reg <- rbind(DEdat_reg, add)
}
DEdat_reg <- DEdat_reg[order(DEdat_reg$Date, DEdat_reg$Province),]   #order in the same way as RIVM data
rownames(DEdat_reg) <- seq(length=nrow(DEdat_reg))
#DailyCases and DailyDeaths have turned into characters
DEdat_reg$DailyCases <- as.integer(DEdat_reg$DailyCases)
DEdat_reg$DailyDeaths <- as.integer(DEdat_reg$DailyDeaths)

#test plots for different provinces
plot(DEdat_reg$Date[DEdat_reg$Province=="Berlin"],DEdat_reg$DailyCases[DEdat_reg$Province=="Berlin"])
plot(DEdat_reg$Date[DEdat_reg$Province=="Berlin"],DEdat_reg$DailyDeaths[DEdat_reg$Province=="Berlin"])



### CREATING CUMULATIVE VARIABLES ###

DEdat_reg$CumCases <- NA
for (i in seq_along(DE_provinces$region_id)) {
  DEdat_reg$CumCases[DEdat_reg$Province==DE_provinces$province_name[i]] <- cumsum(DEdat_reg$DailyCases[DEdat_reg$Province==DE_provinces$province_name[i]])}

DEdat_reg$CumDeaths <- NA
for (i in seq_along(DE_provinces$region_id)) {
  DEdat_reg$CumDeaths[DEdat_reg$Province==DE_provinces$province_name[i]] <- cumsum(DEdat_reg$DailyDeaths[DEdat_reg$Province==DE_provinces$province_name[i]])}

#test plots for different regions
plot(DEdat_reg$Date[DEdat_reg$Province=="Bayern"],DEdat_reg$CumCases[DEdat_reg$Province=="Bayern"])
plot(DEdat_reg$Date[DEdat_reg$Province=="Berlin"],DEdat_reg$CumDeaths[DEdat_reg$Province=="Berlin"])



### CREATING RELATIVE VARIABLES ###

summary(DEdat_reg$Date)
#dataset starts an 07-01-2020 -> need to add 6 days of NAs, for each province
adddate <- seq(from=as.Date("2020-01-01"), to = as.Date("2020-01-06"), by = "days")
nprov <- length(DE_provinces$region_id)
add <- data.frame(Date=rep(adddate, times=nprov),Province=rep(DE_provinces$province_name, each=6),
                  DailyCases=NA,DailyDeaths=NA,CumCases=NA, CumDeaths=NA)
DEdat_reg <- rbind(DEdat_reg,add)
ndays <- length(unique(DEdat_reg$Date))
DEdat_reg <- DEdat_reg[order(DEdat_reg$Date),]
rownames(DEdat_reg) <- 1:nrow(DEdat_reg)

# Cases
DEdat_reg$'CasesLast7Days' <- NA
DEdat_reg$'CasesAvg7Days' <- NA
for (i in 1:nprov) {
  for (j in 7:ndays) {
    DEdat_reg$'CasesLast7Days'[DEdat_reg$Province==DE_provinces$province_name[i]][j] <-  sum(DEdat_reg$DailyCases[DEdat_reg$Province==DE_provinces$province_name[i]][(j-6):j])
    DEdat_reg$'CasesAvg7Days'[DEdat_reg$Province==DE_provinces$province_name[i]][j] <-  mean(DEdat_reg$DailyCases[DEdat_reg$Province==DE_provinces$province_name[i]][(j-6):j]) }}
DEdat_reg$'CasesLast7Days'[1:(6*nprov)] <- 0
DEdat_reg$'CasesAvg7Days'[1:(6*nprov)] <- 0

# Deaths
DEdat_reg$'DeathsLast7Days' <- NA
DEdat_reg$'DeathsAvg7Days' <- NA
for (i in 1:nprov) {
  for (j in 7:ndays) {
    DEdat_reg$'DeathsLast7Days'[DEdat_reg$Province==DE_provinces$province_name[i]][j] <-  sum(DEdat_reg$DailyDeaths[DEdat_reg$Province==DE_provinces$province_name[i]][(j-6):j])
    DEdat_reg$'DeathsAvg7Days'[DEdat_reg$Province==DE_provinces$province_name[i]][j] <-  mean(DEdat_reg$DailyDeaths[DEdat_reg$Province==DE_provinces$province_name[i]][(j-6):j]) }}
DEdat_reg$'DeathsLast7Days'[1:(6*nprov)] <- 0
DEdat_reg$'DeathsAvg7Days'[1:(6*nprov)] <- 0

# test plots for different provinces
plot(DEdat_reg$Date[DEdat_reg$Province=="Bayern"], DEdat_reg$CasesLast7Days[DEdat_reg$Province=="Bayern"])
plot(DEdat_reg$Date[DEdat_reg$Province=="Bayern"], DEdat_reg$CasesAvg7Days[DEdat_reg$Province=="Bayern"])
plot(DEdat_reg$Date[DEdat_reg$Province=="Sachsen"], DEdat_reg$DeathsLast7Days[DEdat_reg$Province=="Sachsen"])
plot(DEdat_reg$Date[DEdat_reg$Province=="Sachsen"], DEdat_reg$DeathsAvg7Days[DEdat_reg$Province=="Sachsen"])






##############################################################################
#                             NATIONAL DATASET
##############################################################################

### CREATING CUMULATIVE VARIABLES ###

DEdat_nat$CumCases <- NA
DEdat_nat$DailyCases[is.na(DEdat_nat$DailyCases)] <- 0
DEdat_nat$CumCases <- cumsum(DEdat_nat$DailyCases)

DEdat_nat$CumDeaths <- NA
DEdat_nat$DailyDeaths[is.na(DEdat_nat$DailyDeaths)] <- 0
DEdat_nat$CumDeaths <- cumsum(DEdat_nat$DailyDeaths)

#test national plots
plot(DEdat_nat$Date,DEdat_nat$CumCases)
plot(DEdat_nat$Date,DEdat_nat$CumDeaths)



### CREATING RELATIVE VARIABLES ###

#prep national dataset
summary(DEdat_nat$Date)   #same dates as department and regional level -> use adddate object
add <- data.frame(Date=adddate,DailyCases=0,DailyDeaths=0,CumCases=0,CumDeaths=0)
DEdat_nat <- rbind(DEdat_nat,add)
ndays <- length(unique(DEdat_nat$Date))
DEdat_nat <- DEdat_nat[order(DEdat_nat$Date),]
rownames(DEdat_nat) <- 1:nrow(DEdat_nat)

# Cases
DEdat_nat$'CasesLast7Days' <- NA
DEdat_nat$'CasesAvg7Days' <- NA
for (j in 7:ndays) {
  DEdat_nat$'CasesLast7Days'[j] <-  sum(DEdat_nat$DailyCases[(j-6):j])
  DEdat_nat$'CasesAvg7Days'[j] <-  mean(DEdat_nat$DailyCases[(j-6):j]) }
DEdat_nat$'CasesLast7Days'[1:6] <- 0
DEdat_nat$'CasesAvg7Days'[1:6] <- 0

# Deaths
DEdat_nat$'DeathsLast7Days' <- NA
DEdat_nat$'DeathsAvg7Days' <- NA
for (j in 7:ndays) {
  DEdat_nat$'DeathsLast7Days'[j] <-  sum(DEdat_nat$DailyDeaths[(j-6):j])
  DEdat_nat$'DeathsAvg7Days'[j] <-  mean(DEdat_nat$DailyDeaths[(j-6):j]) }
DEdat_nat$'DeathsLast7Days'[1:6] <- 0
DEdat_nat$'DeathsAvg7Days'[1:6] <- 0

#test national plots
plot(DEdat_nat$Date,DEdat_nat$DeathsLast7Days)
plot(DEdat_nat$Date,DEdat_nat$DeathsAvg7Days)
plot(DEdat_nat$Date,DEdat_nat$CasesLast7Days)
plot(DEdat_nat$Date,DEdat_nat$CasesAvg7Days)



### ADDING JOHN HOPKINS COUNTRY-LEVEL DATA ###

raw_JHcases <- read.delim("./Data/covid_data/John Hopkins_05.05.2021 - cases.txt", sep=",")
DE_JHcases <- raw_JHcases[raw_JHcases$Country.Region=="Germany",]
DE_JHcases <- subset(DE_JHcases,select = -c(Province.State,Country.Region,Lat,Long))
#turn into long format
DE_JHcases <- gather(DE_JHcases, key=Date, value=JHCumCases)
DE_JHcases$Date <- gsub("X", "",DE_JHcases$Date)
DE_JHcases$Date <- as.Date(DE_JHcases$Date, "%m.%d.%y")

raw_JHdeaths <- read.delim("./Data/covid_data/John Hopkins_05.05.2021 - deaths.txt", sep=",")
#only keep NL (not Aruba etc.)
DE_JHdeaths <- raw_JHdeaths[raw_JHdeaths$Country.Region=="Germany",]
DE_JHdeaths <- subset(DE_JHdeaths,select = -c(Province.State,Country.Region,Lat,Long))
#turn into long format
DE_JHdeaths <- gather(DE_JHdeaths, key=Date, value=JHCumDeaths)
DE_JHdeaths$Date <- gsub("X", "",DE_JHdeaths$Date)
DE_JHdeaths$Date <- as.Date(DE_JHdeaths$Date, "%m.%d.%y")

#merge with national dataset, keeping all dates in JH dataset (starts earlier than nat data)
DEdat_nat <- merge(DEdat_nat, DE_JHcases,by="Date",all.y=T)
DEdat_nat <- merge(DEdat_nat, DE_JHdeaths, by="Date",all.y=T)



### CALCULATING DAILY FIGURES FOR JOHN HOPKINS DATA ###

# DailyCases JH
DEdat_nat$JHDailyCases <- c(0,diff(as.numeric(DEdat_nat$JHCumCases)))
# DailyDeaths JH
DEdat_nat$JHDailyDeaths <- c(0,diff(as.numeric(DEdat_nat$JHCumDeaths)))



### CALCULATING RELATIVE FIGURES FOR JOHN HOPKINS DATA ###

summary(DEdat_nat$Date)
#starts on 22-01-2020 -> need to add 6 days of zeros
adddate <- seq(from=as.Date("2020-01-16"), to = as.Date("2020-01-21"), by = "days")
add <- data.frame(Date=adddate,DailyCases=NA,DailyDeaths=NA, CumCases=NA,CumDeaths=NA, 
                  CasesLast7Days=NA,CasesAvg7Days=NA,DeathsLast7Days=NA,
                  DeathsAvg7Days=NA,JHDailyCases=0,JHDailyDeaths=0,JHCumCases=0,JHCumDeaths=0)
DEdat_nat <- rbind(DEdat_nat,add)
ndays <- length(unique(DEdat_nat$Date))
DEdat_nat <- DEdat_nat[order(DEdat_nat$Date),]
rownames(DEdat_nat) <- 1:nrow(DEdat_nat)
DEdat_nat$JHDailyCases <- as.integer(DEdat_nat$JHDailyCases)
DEdat_nat$JHDailyDeaths <- as.integer(DEdat_nat$JHDailyDeaths)

# Cases
DEdat_nat$'JHCasesLast7Days' <- NA
DEdat_nat$'JHCasesAvg7Days' <- NA
for (j in 7:ndays) {
  DEdat_nat$'JHCasesLast7Days'[j] <-  sum(DEdat_nat$JHDailyCases[(j-6):j])
  DEdat_nat$'JHCasesAvg7Days'[j] <-  mean(DEdat_nat$JHDailyCases[(j-6):j]) }
DEdat_nat$'JHCasesLast7Days'[1:6] <- 0
DEdat_nat$'JHCasesAvg7Days'[1:6] <- 0

# Deaths
DEdat_nat$'JHDeathsLast7Days' <- NA
DEdat_nat$'JHDeathsAvg7Days' <- NA
for (j in 7:ndays) {
  DEdat_nat$'JHDeathsLast7Days'[j] <-  sum(DEdat_nat$JHDailyDeaths[(j-6):j])
  DEdat_nat$'JHDeathsAvg7Days'[j] <-  mean(DEdat_nat$JHDailyDeaths[(j-6):j]) }
DEdat_nat$'JHDeathsLast7Days'[1:6] <- 0
DEdat_nat$'JHDeathsAvg7Days'[1:6] <- 0

#test national plots
plot(DEdat_nat$Date,DEdat_nat$JHDailyCases)
plot(DEdat_nat$Date,DEdat_nat$JHDailyDeaths)
plot(DEdat_nat$Date,DEdat_nat$JHCasesLast7Days)
plot(DEdat_nat$Date,DEdat_nat$JHCasesAvg7Days)
plot(DEdat_nat$Date,DEdat_nat$JHDeathsLast7Days)
plot(DEdat_nat$Date,DEdat_nat$JHDeathsAvg7Days)






#############################
### Save German dataframes
#############################

save(DEdat_reg, file="./Data/prepared_data/DE_regional_prepared_data.RData")
save(DEdat_nat, file="./Data/prepared_data/DE_national_prepared_data.RData")
