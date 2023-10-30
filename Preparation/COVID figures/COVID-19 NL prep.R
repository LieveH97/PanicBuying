##############################################
###                                        ###
###    Preparing NL COVID Health Figures   ###
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
raw <- read.csv("./Data/covid_data/NL RVIM_29.04.2021.csv", sep=";")

#dataframe to manipulate
NLdat_reg <- data.frame(raw)



#######################
# Inspecting data
#######################

str(NLdat_reg)
summary(NLdat_reg)


#Date_of_publication is date of covid case
NLdat_reg$Date <- as.Date(NLdat_reg$Date_of_publication)
NLdat_reg$Province <- factor(NLdat_reg$Province)
str(NLdat_reg)

#one province level is ""
levels(NLdat_reg$Province)
#all values (number of cases, deaths, hospitalizations) are missing
summary(NLdat_reg[NLdat_reg$Province=="",])
#drop observations with this level
NLdat_reg <- NLdat_reg[NLdat_reg$Province!="",]
NLdat_reg$Province <- factor(NLdat_reg$Province)

summary(NLdat_reg)




###########################################
# CREATING REGIONAL AND NATIONAL DATASETS
###########################################

# creating regional dataset
NLdat_reg <- ddply(NLdat_reg,.(Date,Province),summarise,
               DailyCases=sum(na.omit(Total_reported)),DailyDeaths=sum(na.omit(Deceased)),DailyHosp=sum(na.omit(Hospital_admission)))
NL_regions <- data.frame(region_id=1:length(unique(NLdat_reg$Province)),region_name=unique(NLdat_reg$Province))

# creating national dataset
NLdat_nat <- ddply(NLdat_reg,.(Date),summarise,DailyCases=sum(na.omit(DailyCases)),DailyDeaths=sum(na.omit(DailyDeaths)),
                   DailyHosp=sum(na.omit(DailyHosp)))




##############################################################################
#                             REGIONAL DATASET
##############################################################################

### CREATING CUMULATIVE VARIABLES ###

NLdat_reg$CumCases <- NA
for (i in seq_along(NL_regions$region_id)) {
  NLdat_reg$CumCases[NLdat_reg$Province==NL_regions$region_name[i]] <- cumsum(NLdat_reg$DailyCases[NLdat_reg$Province==NL_regions$region_name[i]])}

NLdat_reg$CumDeaths <- NA
for (i in seq_along(NL_regions$region_id)) {
  NLdat_reg$CumDeaths[NLdat_reg$Province==NL_regions$region_name[i]] <- cumsum(NLdat_reg$DailyDeaths[NLdat_reg$Province==NL_regions$region_name[i]])}

NLdat_reg$CumHosp <- NA
for (i in seq_along(NL_regions$region_id)) {
  NLdat_reg$CumHosp[NLdat_reg$Province==NL_regions$region_name[i]] <- cumsum(NLdat_reg$DailyHosp[NLdat_reg$Province==NL_regions$region_name[i]])}

#test plots for different regions
plot(NLdat_reg$Date[NLdat_reg$Province=="Limburg"],NLdat_reg$CumCases[NLdat_reg$Province=="Limburg"])
plot(NLdat_reg$Date[NLdat_reg$Province=="Noord-Brabant"],NLdat_reg$CumDeaths[NLdat_reg$Province=="Noord-Brabant"])
plot(NLdat_reg$Date[NLdat_reg$Province=="Groningen"],NLdat_reg$CumHosp[NLdat_reg$Province=="Groningen"])



### CREATING RELATIVE VARIABLES ###

summary(NLdat_reg)
# dataset starts on 27-02-2020 -> need to add 6 days of zeros, for each province
adddate <- seq(from=as.Date("2020-02-21"), to=as.Date("2020-02-26"), by="days")
nreg <- nrow(NL_regions)
add <- data.frame(Date=rep(adddate, times=nreg),Province=rep(NL_regions$region_name, each=6),
                  DailyCases=NA,DailyDeaths=NA,DailyHosp=NA,CumCases=NA, CumDeaths=NA, CumHosp=NA)
NLdat_reg <- rbind(NLdat_reg,add)
ndays <- length(unique(NLdat_reg$Date))
NLdat_reg <- NLdat_reg[order(NLdat_reg$Date),]
rownames(NLdat_reg) <- 1:nrow(NLdat_reg)

# Cases
NLdat_reg$'CasesLast7Days' <- NA
NLdat_reg$'CasesAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    NLdat_reg$'CasesLast7Days'[NLdat_reg$Province==NL_regions$region_name[i]][j] <-  sum(NLdat_reg$DailyCases[NLdat_reg$Province==NL_regions$region_name[i]][(j-6):j])
    NLdat_reg$'CasesAvg7Days'[NLdat_reg$Province==NL_regions$region_name[i]][j] <-  mean(NLdat_reg$DailyCases[NLdat_reg$Province==NL_regions$region_name[i]][(j-6):j]) }}
NLdat_reg$'CasesLast7Days'[1:(6*nreg)] <- 0
NLdat_reg$'CasesAvg7Days'[1:(6*nreg)] <- 0

# Deaths
NLdat_reg$'DeathsLast7Days' <- NA
NLdat_reg$'DeathsAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    NLdat_reg$'DeathsLast7Days'[NLdat_reg$Province==NL_regions$region_name[i]][j] <- sum(NLdat_reg$DailyDeaths[NLdat_reg$Province==NL_regions$region_name[i]][(j-6):j])
    NLdat_reg$'DeathsAvg7Days'[NLdat_reg$Province==NL_regions$region_name[i]][j] <- mean(NLdat_reg$DailyDeaths[NLdat_reg$Province==NL_regions$region_name[i]][(j-6):j]) }}
NLdat_reg$'DeathsLast7Days'[1:(6*nreg)] <- 0
NLdat_reg$'DeathsAvg7Days'[1:(6*nreg)] <- 0

# Hosp
NLdat_reg$'HospLast7Days' <- NA
NLdat_reg$'HospAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    NLdat_reg$'HospLast7Days'[NLdat_reg$Province==NL_regions$region_name[i]][j] <- sum(NLdat_reg$DailyHosp[NLdat_reg$Province==NL_regions$region_name[i]][(j-6):j])
    NLdat_reg$'HospAvg7Days'[NLdat_reg$Province==NL_regions$region_name[i]][j] <- mean(NLdat_reg$DailyHosp[NLdat_reg$Province==NL_regions$region_name[i]][(j-6):j]) }}
NLdat_reg$'HospLast7Days'[1:(6*nreg)] <- 0
NLdat_reg$'HospAvg7Days'[1:(6*nreg)] <- 0

# test plots for different regions
plot(NLdat_reg$Date[NLdat_reg$Province=="Groningen"], NLdat_reg$CasesAvg7Days[NLdat_reg$Province=="Groningen"])
plot(NLdat_reg$Date[NLdat_reg$Province=="Gelderland"], NLdat_reg$CasesLast7Days[NLdat_reg$Province=="Gelderland"])
plot(NLdat_reg$Date[NLdat_reg$Province=="Overijssel"], NLdat_reg$DeathsAvg7Days[NLdat_reg$Province=="Overijssel"])
plot(NLdat_reg$Date[NLdat_reg$Province=="Limburg"], NLdat_reg$DeathsLast7Days[NLdat_reg$Province=="Limburg"])
plot(NLdat_reg$Date[NLdat_reg$Province=="Utrecht"], NLdat_reg$HospAvg7Days[NLdat_reg$Province=="Utrecht"])
plot(NLdat_reg$Date[NLdat_reg$Province=="Zeeland"], NLdat_reg$HospLast7Days[NLdat_reg$Province=="Zeeland"])





##############################################################################
#                             NATIONAL DATASET
##############################################################################

### CREATING CUMULATIVE VARIABLES ###

NLdat_nat$CumCases <- NA
NLdat_nat$DailyCases[is.na(NLdat_nat$DailyCases)] <- 0
NLdat_nat$CumCases <- cumsum(NLdat_nat$DailyCases)

NLdat_nat$CumDeaths <- NA
NLdat_nat$DailyDeaths[is.na(NLdat_nat$DailyDeaths)] <- 0
NLdat_nat$CumDeaths <- cumsum(NLdat_nat$DailyDeaths)

NLdat_nat$CumHosp <- NA
NLdat_nat$DailyHosp[is.na(NLdat_nat$DailyHosp)] <- 0
NLdat_nat$CumHosp <- cumsum(NLdat_nat$DailyHosp)

#test national plots
plot(NLdat_nat$Date,NLdat_nat$CumCases)
plot(NLdat_nat$Date,NLdat_nat$CumDeaths)
plot(NLdat_nat$Date,NLdat_nat$CumHosp)


### CREATING RELATIVE VARIABLES ###
#prep national dataset
summary(NLdat_nat$Date)   #same dates as department and regional level -> use adddate object
add <- data.frame(Date=adddate,DailyCases=0,DailyDeaths=0,DailyHosp=0,CumCases=0,CumDeaths=0,CumHosp=0)
NLdat_nat <- rbind(NLdat_nat,add)
ndays <- length(unique(NLdat_nat$Date))
NLdat_nat <- NLdat_nat[order(NLdat_nat$Date),]
rownames(NLdat_nat) <- 1:nrow(NLdat_nat)

# Cases
NLdat_nat$'CasesLast7Days' <- NA
NLdat_nat$'CasesAvg7Days' <- NA
for (j in 7:ndays) {
  NLdat_nat$'CasesLast7Days'[j] <-  sum(NLdat_nat$DailyCases[(j-6):j])
  NLdat_nat$'CasesAvg7Days'[j] <-  mean(NLdat_nat$DailyCases[(j-6):j]) }
NLdat_nat$'CasesLast7Days'[1:6] <- 0
NLdat_nat$'CasesAvg7Days'[1:6] <- 0

# Deaths
NLdat_nat$'DeathsLast7Days' <- NA
NLdat_nat$'DeathsAvg7Days' <- NA
for (j in 7:ndays) {
  NLdat_nat$'DeathsLast7Days'[j] <-  sum(NLdat_nat$DailyDeaths[(j-6):j])
  NLdat_nat$'DeathsAvg7Days'[j] <-  mean(NLdat_nat$DailyDeaths[(j-6):j]) }
NLdat_nat$'DeathsLast7Days'[1:6] <- 0
NLdat_nat$'DeathsAvg7Days'[1:6] <- 0

#Hosp
NLdat_nat$'HospLast7Days' <- NA
NLdat_nat$'HospAvg7Days' <- NA
for (j in 7:ndays) {
  NLdat_nat$'HospLast7Days'[j] <-  sum(NLdat_nat$DailyHosp[(j-6):j])
  NLdat_nat$'HospAvg7Days'[j] <-  mean(NLdat_nat$DailyHosp[(j-6):j]) }
NLdat_nat$'HospLast7Days'[1:6] <- 0
NLdat_nat$'HospAvg7Days'[1:6] <- 0

#test national plots
plot(NLdat_nat$Date,NLdat_nat$DeathsLast7Days)
plot(NLdat_nat$Date,NLdat_nat$DeathsAvg7Days)
plot(NLdat_nat$Date,NLdat_nat$CasesLast7Days)
plot(NLdat_nat$Date,NLdat_nat$CasesAvg7Days)
plot(NLdat_nat$Date,NLdat_nat$HospLast7Days)
plot(NLdat_nat$Date,NLdat_nat$HospAvg7Days)


### ADDING JOHN HOPKINS COUNTRY-LEVEL DATA ###


raw_JHcases <- read.delim("./Data/covid_data/John Hopkins_05.05.2021 - cases.txt", sep=",")
#only keep Netherlands (not Aruba etc.)
NL_JHcases <- raw_JHcases[raw_JHcases$Province.State=="" & raw_JHcases$Country.Region=="Netherlands",]
NL_JHcases <- subset(NL_JHcases,select = -c(Province.State,Country.Region,Lat,Long))
#turn into long format
NL_JHcases <- gather(NL_JHcases, key=Date, value=JHCumCases)
NL_JHcases$Date <- gsub("X", "",NL_JHcases$Date)
NL_JHcases$Date <- as.Date(NL_JHcases$Date, "%m.%d.%y")

raw_JHdeaths <- read.delim("./Data/covid_data/John Hopkins_05.05.2021 - deaths.txt", sep=",")
#only keep NL (not Aruba etc.)
NL_JHdeaths <- raw_JHdeaths[raw_JHdeaths$Province.State=="" & raw_JHdeaths$Country.Region=="Netherlands",]
NL_JHdeaths <- subset(NL_JHdeaths,select = -c(Province.State,Country.Region,Lat,Long))
#turn into long format
NL_JHdeaths <- gather(NL_JHdeaths, key=Date, value=JHCumDeaths)
NL_JHdeaths$Date <- gsub("X", "",NL_JHdeaths$Date)
NL_JHdeaths$Date <- as.Date(NL_JHdeaths$Date, "%m.%d.%y")

#merge with national dataset, keeping all dates in JH dataset (starts earlier than nat data)
NLdat_nat <- merge(NLdat_nat, NL_JHcases,by="Date",all.y=T)
NLdat_nat <- merge(NLdat_nat, NL_JHdeaths, by="Date",all.y=T)



### CALCULATING DAILY FIGURES FOR JOHN HOPKINS DATA ###

# DailyCases JH
NLdat_nat$JHDailyCases <- c(0,diff(as.numeric(NLdat_nat$JHCumCases)))
# DailyDeaths JH
NLdat_nat$JHDailyDeaths <- c(0,diff(as.numeric(NLdat_nat$JHCumDeaths)))



### CALCULATING RELATIVE FIGURES FOR JOHN HOPKINS DATA ###

summary(NLdat_nat$Date)
#starts on 22-01-2020 -> need to add 6 days of zeros
adddate <- seq(from=as.Date("2020-01-16"), to = as.Date("2020-01-21"), by = "days")
add <- data.frame(Date=adddate,DailyCases=NA,DailyDeaths=NA,DailyHosp=NA, CumCases=NA,
                  CumDeaths=NA, CumHosp=NA, CasesLast7Days=NA,CasesAvg7Days=NA,
                  DeathsLast7Days=NA,DeathsAvg7Days=NA,HospLast7Days=NA,
                  HospAvg7Days=NA,JHDailyCases=0,JHDailyDeaths=0,JHCumCases=0,JHCumDeaths=0)
NLdat_nat <- rbind(NLdat_nat,add)
ndays <- length(unique(NLdat_nat$Date))
NLdat_nat <- NLdat_nat[order(NLdat_nat$Date),]
rownames(NLdat_nat) <- 1:nrow(NLdat_nat)
NLdat_nat$JHDailyCases <- as.integer(NLdat_nat$JHDailyCases)
NLdat_nat$JHDailyDeaths <- as.integer(NLdat_nat$JHDailyDeaths)

# Cases
NLdat_nat$'JHCasesLast7Days' <- NA
NLdat_nat$'JHCasesAvg7Days' <- NA
for (j in 7:ndays) {
  NLdat_nat$'JHCasesLast7Days'[j] <-  sum(NLdat_nat$JHDailyCases[(j-6):j])
  NLdat_nat$'JHCasesAvg7Days'[j] <-  mean(NLdat_nat$JHDailyCases[(j-6):j]) }
NLdat_nat$'JHCasesLast7Days'[1:6] <- 0
NLdat_nat$'JHCasesAvg7Days'[1:6] <- 0

# Deaths
NLdat_nat$'JHDeathsLast7Days' <- NA
NLdat_nat$'JHDeathsAvg7Days' <- NA
for (j in 7:ndays) {
  NLdat_nat$'JHDeathsLast7Days'[j] <-  sum(NLdat_nat$JHDailyDeaths[(j-6):j])
  NLdat_nat$'JHDeathsAvg7Days'[j] <-  mean(NLdat_nat$JHDailyDeaths[(j-6):j]) }
NLdat_nat$'JHDeathsLast7Days'[1:6] <- 0
NLdat_nat$'JHDeathsAvg7Days'[1:6] <- 0

#test national plots
plot(NLdat_nat$Date,NLdat_nat$JHDailyCases)
plot(NLdat_nat$Date,NLdat_nat$JHDailyDeaths)
plot(NLdat_nat$Date,NLdat_nat$JHCasesLast7Days)
plot(NLdat_nat$Date,NLdat_nat$JHCasesAvg7Days)
plot(NLdat_nat$Date,NLdat_nat$JHDeathsLast7Days)
plot(NLdat_nat$Date,NLdat_nat$JHDeathsAvg7Days)








#############################
### Save NL dataframes
#############################

save(NLdat_reg, file="./Data/prepared_data/NL_province_prepared_data.RData")
save(NLdat_nat, file="./Data/prepared_data/NL_national_prepared_data.RData")
