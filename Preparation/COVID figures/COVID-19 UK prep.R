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
list.of.packages <- c("plyr", "readxl")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)


####################
# Loading data
####################

#raw dataset
raw_cases <- read.csv("./Data/covid_data/UK PHE_04.05.2021 - cases.csv")
raw_deaths <- read.csv("./Data/covid_data/UK PHE_04.05.2021 - death.csv")
raw_hosp <- read.csv("./Data/covid_data/UK PHE_04.05.2021 - hosp.csv")

#dataframe to manipulate
UKcases <- data.frame(raw_cases)
UKhosp <- data.frame(raw_hosp)
UKdeaths <- data.frame(raw_deaths)



#######################
# Inspecting data
#######################

### CASES DATASET ###
str(UKcases)
summary(UKcases)
#Convert the relevant variables to correct type
UKcases$date <- as.Date(UKcases$date)
UKcases$areaName <- factor(UKcases$areaName)
levels(UKcases$areaName)
#List with all areas
UK_areas <- data.frame(area_id=1:length(unique(UKcases$areaName)),area_name=unique(UKcases$areaName))


### HOSPITALIZATIONS DATASET ###
str(UKhosp)
summary(UKhosp)
#Convert the relevant variables to correct type
UKhosp$date <- as.Date(UKhosp$date)
UKhosp$areaName <- factor(UKhosp$areaName)


### DEATHS DATASET ###
str(UKdeaths)
summary(UKdeaths)
#Convert the relevant variables to correct type
UKdeaths$date <- as.Date(UKdeaths$date)
UKdeaths$areaName <- factor(UKdeaths$areaName)



##########################################
# CREATING REGIONAL AND NATIONAL DATASETS
#########################################

### REGIONAL DATASET ###
#bring together all three datasets -> match based on area+date combo
UKdat_reg <- merge(UKcases,UKdeaths,by=c("areaType","areaName","areaCode","date"),all=T)
UKdat_reg <- merge(UKdat_reg, UKhosp, by=c("areaType","areaName","areaCode","date"),all=T)
rm(list=c('UKcases','UKhosp','UKdeaths'))
UKdat_reg <- rename(UKdat_reg, replace = c("areaName"="Region", "date"="Date","newCasesByPublishDate"="DailyCases",
                                   "cumCasesByPublishDate"="CumCases","newDeaths28DaysByPublishDate"="DailyDeaths",
                                   "cumDeaths28DaysByPublishDate"="CumDeaths","newAdmissions"="DailyHosp","cumAdmissions"="CumHosp"))
UKdat_reg <- UKdat_reg[c("Date","Region","DailyCases","DailyDeaths","DailyHosp","CumCases","CumDeaths","CumHosp")]

### NATIONAL DATASET ###
#ddply doesn't sum data for dates where NA for one region
UKdat_reg$DailyCases[is.na(UKdat_reg$DailyCases)] <- 0
UKdat_reg$DailyDeaths[is.na(UKdat_reg$DailyDeaths)] <- 0
UKdat_reg$DailyHosp[is.na(UKdat_reg$DailyHosp)] <- 0
UKdat_nat <- ddply(UKdat_reg, .(Date), summarize, DailyCases=sum(na.omit(DailyCases)), DailyDeaths=sum(na.omit(DailyDeaths)),
                   DailyHosp=sum(na.omit(DailyHosp)))




############################################################################
#                         PREPARING REGIONAL DATASET
############################################################################

### INCLUDING NON-OBSERVATIONS ###

#now, only observations for dates where a case or death or hospitalization was reported in the region
#loop over all dates
summary(UKdat_reg$Date)
all.dates <- seq(as.Date("2020-01-25",format="%Y-%m-%d"), as.Date("2021-05-03",format="%Y-%m-%d"), by="days")
#loop over regions -> create new rows with missing date + region name
for (i in seq_along(UK_areas$area_name)) {
  miss.dates <- subset(all.dates, !(all.dates %in% UKdat_reg$Date[UKdat_reg$Region==UK_areas$area_name[i]]))
  miss.dates <- as.Date(miss.dates,format="%Y-%m-%d")
  add <- cbind(as.character(miss.dates), as.character(UK_areas$area_name[i]), 0, 0,0,0,0,0)
  colnames(add) <- c("Date", "Region","DailyCases", "DailyDeaths","DailyHosp","CumCases","CumDeaths","CumHosp")
  UKdat_reg <- rbind(UKdat_reg, add)
}
UKdat_reg <- UKdat_reg[order(UKdat_reg$Date, UKdat_reg$Region),]
rownames(UKdat_reg) <- seq(length=nrow(UKdat_reg))
str(UKdat_reg)
#Cases/Deaths/Hosp have turned into characters
UKdat_reg$DailyCases <- as.integer(UKdat_reg$DailyCases)
UKdat_reg$DailyDeaths <- as.integer(UKdat_reg$DailyDeaths)
UKdat_reg$DailyHosp <- as.integer(UKdat_reg$DailyHosp)
UKdat_reg$CumCases <- as.integer(UKdat_reg$CumCases)
UKdat_reg$CumDeaths <- as.integer(UKdat_reg$CumDeaths)
UKdat_reg$CumHosp <- as.integer(UKdat_reg$CumHosp)


### CREATING RELATIVE VARIABLES: CASES/DEATHS/HOSP LAST 7 DAYS & AVERAGE 7 DAYS

summary(UKdat_reg)
#dataset starts on 31-01-2020 -> need to add 6 days of zeros, for each region
adddate <- seq(from=as.Date("2020-01-25"), to = as.Date("2020-01-30"), by = "days")
nreg <- length(UK_areas$area_id)
add <- data.frame(Date=rep(adddate, times=nreg),Region=rep(UK_areas$area_name, each=6),
                  DailyCases=NA,DailyDeaths=NA,CumCases=NA, CumDeaths=NA, DailyHosp=NA, CumHosp=NA)
UKdat_reg <- rbind(UKdat_reg,add)
ndays <- length(unique(UKdat_reg$Date))
UKdat_reg <- UKdat_reg[order(UKdat_reg$Date),]
rownames(UKdat_reg) <- 1:nrow(UKdat_reg)

# Cases
UKdat_reg$'CasesLast7Days' <- NA
UKdat_reg$'CasesAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    UKdat_reg$'CasesLast7Days'[UKdat_reg$Region==UK_areas$area_name[i]][j] <-  sum(UKdat_reg$DailyCases[UKdat_reg$Region==UK_areas$area_name[i]][(j-6):j])
    UKdat_reg$'CasesAvg7Days'[UKdat_reg$Region==UK_areas$area_name[i]][j] <-  mean(UKdat_reg$DailyCases[UKdat_reg$Region==UK_areas$area_name[i]][(j-6):j]) }}
UKdat_reg$'CasesLast7Days'[1:(6*nreg)] <- 0
UKdat_reg$'CasesAvg7Days'[1:(6*nreg)] <- 0

# Deaths
UKdat_reg$'DeathsLast7Days' <- NA
UKdat_reg$'DeathsAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    UKdat_reg$'DeathsLast7Days'[UKdat_reg$Region==UK_areas$area_name[i]][j] <-  sum(UKdat_reg$DailyDeaths[UKdat_reg$Region==UK_areas$area_name[i]][(j-6):j])
    UKdat_reg$'DeathsAvg7Days'[UKdat_reg$Region==UK_areas$area_name[i]][j] <-  mean(UKdat_reg$DailyDeaths[UKdat_reg$Region==UK_areas$area_name[i]][(j-6):j]) }}
UKdat_reg$'DeathsLast7Days'[1:(6*nreg)] <- 0
UKdat_reg$'DeathsAvg7Days'[1:(6*nreg)] <- 0

# Hosp
UKdat_reg$'HospLast7Days' <- NA
UKdat_reg$'HospAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    UKdat_reg$'HospLast7Days'[UKdat_reg$Region==UK_areas$area_name[i]][j] <-  sum(UKdat_reg$DailyHosp[UKdat_reg$Region==UK_areas$area_name[i]][(j-6):j])
    UKdat_reg$'HospAvg7Days'[UKdat_reg$Region==UK_areas$area_name[i]][j] <-  mean(UKdat_reg$DailyHosp[UKdat_reg$Region==UK_areas$area_name[i]][(j-6):j]) }}
UKdat_reg$'HospLast7Days'[1:(6*nreg)] <- 0
UKdat_reg$'HospAvg7Days'[1:(6*nreg)] <- 0

# test plots
plot(UKdat_reg$Date[UKdat_reg$Region=="England"], UKdat_reg$CasesLast7Days[UKdat_reg$Region=="England"])
plot(UKdat_reg$Date[UKdat_reg$Region=="Wales"], UKdat_reg$CasesAvg7Days[UKdat_reg$Region=="Wales"])
plot(UKdat_reg$Date[UKdat_reg$Region=="Northern Ireland"], UKdat_reg$DeathsLast7Days[UKdat_reg$Region=="Northern Ireland"])
plot(UKdat_reg$Date[UKdat_reg$Region=="Scotland"], UKdat_reg$DeathsAvg7Days[UKdat_reg$Region=="Scotland"])
plot(UKdat_reg$Date[UKdat_reg$Region=="England"], UKdat_reg$HospLast7Days[UKdat_reg$Region=="England"])
plot(UKdat_reg$Date[UKdat_reg$Region=="Wales"], UKdat_reg$HospLast7Days[UKdat_reg$Region=="Wales"])





############################################################################
#                         PREPARING NATIONAL DATASET
############################################################################

### CREATING CUMULATIVE VARIABLES ###

UKdat_nat$CumCases <- NA
UKdat_nat$DailyCases[is.na(UKdat_nat$DailyCases)] <- 0
UKdat_nat$CumCases <- cumsum(UKdat_nat$DailyCases)

UKdat_nat$CumDeaths <- NA
UKdat_nat$DailyDeaths[is.na(UKdat_nat$DailyDeaths)] <- 0
UKdat_nat$CumDeaths <- cumsum(UKdat_nat$DailyDeaths)

UKdat_nat$CumHosp <- NA
UKdat_nat$DailyHosp[is.na(UKdat_nat$DailyHosp)] <- 0
UKdat_nat$CumHosp <- cumsum(UKdat_nat$DailyHosp)

#test national plots
plot(UKdat_nat$Date,UKdat_nat$CumCases)
plot(UKdat_nat$Date,UKdat_nat$CumDeaths)
plot(UKdat_nat$Date,UKdat_nat$CumHosp)




### CREATING RELATIVE VARIABLES ###

#prep national dataset
summary(UKdat_nat$Date)   #same dates as regional level -> use adddate object
add <- data.frame(Date=adddate,DailyCases=0,DailyDeaths=0,DailyHosp=0,CumCases=0,
                  CumDeaths=0, CumHosp=0)
UKdat_nat <- rbind(UKdat_nat,add)
ndays <- length(unique(UKdat_nat$Date))
UKdat_nat <- UKdat_nat[order(UKdat_nat$Date),]
rownames(UKdat_nat) <- 1:nrow(UKdat_nat)

# Cases
UKdat_nat$'CasesLast7Days' <- NA
UKdat_nat$'CasesAvg7Days' <- NA
for (j in 7:ndays) {
  UKdat_nat$'CasesLast7Days'[j] <-  sum(UKdat_nat$DailyCases[(j-6):j])
  UKdat_nat$'CasesAvg7Days'[j] <-  mean(UKdat_nat$DailyCases[(j-6):j]) }
UKdat_nat$'CasesLast7Days'[1:6] <- 0
UKdat_nat$'CasesAvg7Days'[1:6] <- 0

# Deaths
UKdat_nat$'DeathsLast7Days' <- NA
UKdat_nat$'DeathsAvg7Days' <- NA
for (j in 7:ndays) {
  UKdat_nat$'DeathsLast7Days'[j] <-  sum(UKdat_nat$DailyDeaths[(j-6):j])
  UKdat_nat$'DeathsAvg7Days'[j] <-  mean(UKdat_nat$DailyDeaths[(j-6):j]) }
UKdat_nat$'DeathsLast7Days'[1:6] <- 0
UKdat_nat$'DeathsAvg7Days'[1:6] <- 0

#Hosp
UKdat_nat$'HospLast7Days' <- NA
UKdat_nat$'HospAvg7Days' <- NA
for (j in 7:ndays) {
  UKdat_nat$'HospLast7Days'[j] <-  sum(UKdat_nat$DailyHosp[(j-6):j])
  UKdat_nat$'HospAvg7Days'[j] <-  mean(UKdat_nat$DailyHosp[(j-6):j]) }
UKdat_nat$'HospLast7Days'[1:6] <- 0
UKdat_nat$'HospAvg7Days'[1:6] <- 0

#test national plots
plot(UKdat_nat$Date,UKdat_nat$DeathsLast7Days)
plot(UKdat_nat$Date,UKdat_nat$DeathsAvg7Days)
plot(UKdat_nat$Date,UKdat_nat$CasesLast7Days)
plot(UKdat_nat$Date,UKdat_nat$CasesAvg7Days)
plot(UKdat_nat$Date,UKdat_nat$HospLast7Days)
plot(UKdat_nat$Date,UKdat_nat$HospAvg7Days)



### ADDING JOHN HOPKINS DATA ###

raw_JHcases <- read.delim("./Data/covid_data/John Hopkins_05.05.2021 - cases.txt", sep=",")
#only keep UK (not Gibraltar etc.)
UK_JHcases <- raw_JHcases[raw_JHcases$Province.State=="" & raw_JHcases$Country.Region=="United Kingdom",]
UK_JHcases <- subset(UK_JHcases,select = -c(Province.State,Country.Region,Lat,Long))
#turn into long format
UK_JHcases <- gather(UK_JHcases, key=Date, value=JHCumCases)
UK_JHcases$Date <- gsub("X", "",UK_JHcases$Date)
UK_JHcases$Date <- as.Date(UK_JHcases$Date, "%m.%d.%y")

raw_JHdeaths <- read.delim("./Data/covid_data/John Hopkins_05.05.2021 - deaths.txt", sep=",")
#only keep UK (not Gibraltar etc.)
UK_JHdeaths <- raw_JHdeaths[raw_JHdeaths$Province.State=="" & raw_JHdeaths$Country.Region=="United Kingdom",]
UK_JHdeaths <- subset(UK_JHdeaths,select = -c(Province.State,Country.Region,Lat,Long))
#turn into long format
UK_JHdeaths <- gather(UK_JHdeaths, key=Date, value=JHCumDeaths)
UK_JHdeaths$Date <- gsub("X", "",UK_JHdeaths$Date)
UK_JHdeaths$Date <- as.Date(UK_JHdeaths$Date, "%m.%d.%y")

#merge with national dataset, keeping all dates in JH dataset (starts earlier than nat data)
UKdat_nat <- merge(UKdat_nat, UK_JHcases,by="Date",all.y=T)
UKdat_nat <- merge(UKdat_nat, UK_JHdeaths, by="Date",all.y=T)


### CALCULATING DAILY FIGURES FOR JOHN HOPKINS DATA ###

# DailyCases JH
UKdat_nat$JHDailyCases <- c(0,diff(as.numeric(UKdat_nat$JHCumCases)))

# DailyDeaths JH
UKdat_nat$JHDailyDeaths <- c(0,diff(as.numeric(UKdat_nat$JHCumDeaths)))


### CALCULATING RELATIVE FIGURES FOR JOHN HOPKINS DATA ###

summary(UKdat_nat$Date)
#starts on 22-01-2020 -> need to add 6 days of zeros
adddate <- seq(from=as.Date("2020-01-16"), to = as.Date("2020-01-21"), by = "days")
add <- data.frame(Date=adddate,DailyCases=NA,DailyDeaths=NA,DailyHosp=NA, CumCases=NA,
                  CumDeaths=NA, CumHosp=NA, CasesLast7Days=NA,CasesAvg7Days=NA,
                  DeathsLast7Days=NA,DeathsAvg7Days=NA,HospLast7Days=NA,
                  HospAvg7Days=NA,JHDailyCases=0,JHDailyDeaths=0,JHCumCases=0,JHCumDeaths=0)
UKdat_nat <- rbind(UKdat_nat,add)
ndays <- length(unique(UKdat_nat$Date))
UKdat_nat <- UKdat_nat[order(UKdat_nat$Date),]
rownames(UKdat_nat) <- 1:nrow(UKdat_nat)
UKdat_nat$JHDailyCases <- as.integer(UKdat_nat$JHDailyCases)
UKdat_nat$JHDailyDeaths <- as.integer(UKdat_nat$JHDailyDeaths)

# Cases
UKdat_nat$'JHCasesLast7Days' <- NA
UKdat_nat$'JHCasesAvg7Days' <- NA
for (j in 7:ndays) {
  UKdat_nat$'JHCasesLast7Days'[j] <-  sum(UKdat_nat$JHDailyCases[(j-6):j])
  UKdat_nat$'JHCasesAvg7Days'[j] <-  mean(UKdat_nat$JHDailyCases[(j-6):j]) }
UKdat_nat$'JHCasesLast7Days'[1:6] <- 0
UKdat_nat$'JHCasesAvg7Days'[1:6] <- 0

# Deaths
UKdat_nat$'JHDeathsLast7Days' <- NA
UKdat_nat$'JHDeathsAvg7Days' <- NA
for (j in 7:ndays) {
  UKdat_nat$'JHDeathsLast7Days'[j] <-  sum(UKdat_nat$JHDailyDeaths[(j-6):j])
  UKdat_nat$'JHDeathsAvg7Days'[j] <-  mean(UKdat_nat$JHDailyDeaths[(j-6):j]) }
UKdat_nat$'JHDeathsLast7Days'[1:6] <- 0
UKdat_nat$'JHDeathsAvg7Days'[1:6] <- 0

#test national plots
plot(UKdat_nat$Date,UKdat_nat$JHDailyCases)
plot(UKdat_nat$Date,UKdat_nat$JHDailyDeaths)
plot(UKdat_nat$Date,UKdat_nat$JHCasesLast7Days)
plot(UKdat_nat$Date,UKdat_nat$JHCasesAvg7Days)
plot(UKdat_nat$Date,UKdat_nat$JHDeathsLast7Days)
plot(UKdat_nat$Date,UKdat_nat$JHDeathsAvg7Days)






#############################
### Save UKagg dataframes
#############################

save(UKdat_reg, file="./Data/prepared_data/UK_area_prepared_data.RData")
save(UKdat_reg, file="./Data/prepared_data/UK_national_prepared_data.RData")
