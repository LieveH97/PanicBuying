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
list.of.packages <- c("plyr","tidyr")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)


####################
# Loading data
####################

#raw dataset
raw_dep <- read.csv("./Data/covid_data/FR SPF_03.05.2021 - departement.csv", sep=",")
raw_nat <- read.csv("./Data/covid_data/FR SPF_03.05.2021 - national.csv", sep=",")


#dataframe to manipulate
FRdep <- data.frame(raw_dep)
FRdat_nat <- data.frame(raw_nat)



#######################
# Inspecting data
#######################


#incid_hosp = number of new admissions in hosp in last 24 hours
#incid_dhosp = number of new deaths in hosp in last 24 hours
   #deaths elsewhere?? not reported in newspapers either
#pos = number of people declared positive
   # not the same as conf = number of confirmed cases!
   # newspapers report cases, not "pos"
#incid_rea = number of new admissions in ICU in last 24 hours

# construct three datasets:
   # department level: Deaths + Hosp + ICU
   # regional level: Deaths + Hosp + ICU
   # national level: Cases + Deaths + Hosp + ICU



str(FRdep)
summary(FRdep)

FRdep$date <- as.Date(FRdep$date)
FRdep$lib_dep <- factor(FRdep$lib_dep)
levels(FRdep$lib_dep)
# 101 departements -> also includes territories off mainland
FRdep$lib_reg <- factor(FRdep$lib_reg)
levels(FRdep$lib_reg)
FR_regions <- data.frame(region_id=1:length(unique(FRdep$lib_reg)),
                         region_name=levels(FRdep$lib_reg))
FR_depart <- data.frame(depart_id=1:length(unique(FRdep$lib_dep)),
                         depart_name=levels(FRdep$lib_dep))

#translate necessary variables to English
FRdep <- rename(FRdep, replace = c("date"="Date", "lib_reg"="Region","lib_dep"="Department",
                                  "incid_dchosp"="DailyDeaths", "incid_hosp"="DailyHosp","incid_rea"="DailyICU"))


# prepare national dataset
str(FRdat_nat)
summary(FRdat_nat)

FRdat_nat$date <- as.Date(FRdat_nat$date)
FRdat_nat <- rename(FRdat_nat, replace = c("date"="Date","incid_hosp"="DailyHosp","incid_dchosp"="DailyDeaths",
                                           "conf_j1"="DailyCases","incid_rea"="DailyICU"))


#create datasets on three-levels: department - region - country
FRdat_dep <- FRdep[,c("Date","Department","DailyDeaths","DailyHosp","DailyICU")]
FRdat_reg <- ddply(FRdep,.(Date,Region),summarise,
                   DailyDeaths=sum(na.omit(DailyDeaths)),DailyHosp=sum(na.omit(DailyHosp)),DailyICU=sum(na.omit(DailyICU)))
FRdat_nat <- FRdat_nat[,c("Date","DailyCases","DailyDeaths","DailyHosp","DailyICU")]
rm('FRdep')



############################################################################
#                       DEPARTMENT-LEVEL DATASET
############################################################################

### NEW VARIABLES: CUMDEATHS, CUMHOSP, CUMICU ###

FRdat_dep$CumDeaths <- NA
FRdat_dep$DailyDeaths[is.na(FRdat_dep$DailyDeaths)] <- 0
#loop over all departments in FR -> take cumulative sum of deaths of that department
for (i in seq_along(FR_depart$depart_id)) {
  FRdat_dep$CumDeaths[FRdat_dep$Department==FR_depart$depart_name[i]] <- cumsum(FRdat_dep$DailyDeaths[FRdat_dep$Department==FR_depart$depart_name[i]])}

FRdat_dep$CumHosp <- NA
FRdat_dep$DailyHosp[is.na(FRdat_dep$DailyHosp)] <- 0
for (i in seq_along(FR_depart$depart_id)) {
  FRdat_dep$CumHosp[FRdat_dep$Department==FR_depart$depart_name[i]] <- cumsum(FRdat_dep$DailyHosp[FRdat_dep$Department==FR_depart$depart_name[i]])}

FRdat_dep$CumICU <- NA
FRdat_dep$DailyICU[is.na(FRdat_dep$DailyICU)] <- 0
for (i in seq_along(FR_depart$depart_id)) {
  FRdat_dep$CumICU[FRdat_dep$Department==FR_depart$depart_name[i]] <- cumsum(FRdat_dep$DailyICU[FRdat_dep$Department==FR_depart$depart_name[i]])}

#test department plots
plot(FRdat_dep$Date[FRdat_dep$Department=="Somme"],FRdat_dep$CumDeaths[FRdat_dep$Department=="Somme"])
plot(FRdat_dep$Date[FRdat_dep$Department=="Ardennes"],FRdat_dep$CumHosp[FRdat_dep$Department=="Ardennes"])
plot(FRdat_dep$Date[FRdat_dep$Department=="Loire"],FRdat_dep$CumICU[FRdat_dep$Department=="Loire"])



### NEW VARIABLES: DEATHS/HOSP/ICU LAST7DAYS & AVG7DAYS ###

#prep department dataset
summary(FRdat_dep$Date)
#dataset begins on 18-03-2020 -> need to add 6 days of zeros for every department
adddate <- seq(from=as.Date("2020-03-12"), to = as.Date("2020-03-17"), by = "days")
ndep <- length(FR_depart$depart_id)
add <- data.frame(Date=rep(adddate, times=ndep),Department=rep(FR_depart$depart_name, each=6),
                  DailyDeaths=0,DailyHosp=0,DailyICU=0,CumDeaths=0, CumHosp=0, CumICU=0)
FRdat_dep <- rbind(FRdat_dep,add)
ndays <- length(unique(FRdat_dep$Date))
FRdat_dep <- FRdat_dep[order(FRdat_dep$Date),]
rownames(FRdat_dep) <- 1:nrow(FRdat_dep)

# Deaths
FRdat_dep$'DeathsLast7Days' <- NA
FRdat_dep$'DeathsAvg7Days' <- NA
for (i in 1:ndep) {
  for (j in 7:ndays) {
    FRdat_dep$'DeathsLast7Days'[FRdat_dep$Department==FR_depart$depart_name[i]][j] <-  sum(FRdat_dep$DailyDeaths[FRdat_dep$Department==FR_depart$depart_name[i]][(j-6):j])
    FRdat_dep$'DeathsAvg7Days'[FRdat_dep$Department==FR_depart$depart_name[i]][j] <-  mean(FRdat_dep$DailyDeaths[FRdat_dep$Department==FR_depart$depart_name[i]][(j-6):j]) }}
FRdat_dep$'DeathsLast7Days'[1:(6*ndep)] <- 0
FRdat_dep$'DeathsAvg7Days'[1:(6*ndep)] <- 0

# Hosp
FRdat_dep$'HospLast7Days' <- NA
FRdat_dep$'HospAvg7Days' <- NA
for (i in 1:ndep) {
  for (j in 7:ndays) {
    FRdat_dep$'HospLast7Days'[FRdat_dep$Department==FR_depart$depart_name[i]][j] <-  sum(FRdat_dep$DailyHosp[FRdat_dep$Department==FR_depart$depart_name[i]][(j-6):j])
    FRdat_dep$'HospAvg7Days'[FRdat_dep$Department==FR_depart$depart_name[i]][j] <-  mean(FRdat_dep$DailyHosp[FRdat_dep$Department==FR_depart$depart_name[i]][(j-6):j]) }}
FRdat_dep$'HospLast7Days'[1:(6*ndep)] <- 0
FRdat_dep$'HospAvg7Days'[1:(6*ndep)] <- 0

# ICU
FRdat_dep$'ICULast7Days' <- NA
FRdat_dep$'ICUAvg7Days' <- NA
for (i in 1:ndep) {
  for (j in 7:ndays) {
    FRdat_dep$'ICULast7Days'[FRdat_dep$Department==FR_depart$depart_name[i]][j] <-  sum(FRdat_dep$DailyICU[FRdat_dep$Department==FR_depart$depart_name[i]][(j-6):j])
    FRdat_dep$'ICUAvg7Days'[FRdat_dep$Department==FR_depart$depart_name[i]][j] <-  mean(FRdat_dep$DailyICU[FRdat_dep$Department==FR_depart$depart_name[i]][(j-6):j]) }}
FRdat_dep$'ICULast7Days'[1:(6*ndep)] <- 0
FRdat_dep$'ICUAvg7Days'[1:(6*ndep)] <- 0

#test department plots
plot(FRdat_dep$Date[FRdat_dep$Department=="Somme"],FRdat_dep$DeathsLast7Days[FRdat_dep$Department=="Somme"])
plot(FRdat_dep$Date[FRdat_dep$Department=="Somme"],FRdat_dep$DeathsAvg7Days[FRdat_dep$Department=="Somme"])
plot(FRdat_dep$Date[FRdat_dep$Department=="Ardennes"],FRdat_dep$HospLast7Days[FRdat_dep$Department=="Ardennes"])
plot(FRdat_dep$Date[FRdat_dep$Department=="Ardennes"],FRdat_dep$HospAvg7Days[FRdat_dep$Department=="Ardennes"])
plot(FRdat_dep$Date[FRdat_dep$Department=="Loire"],FRdat_dep$ICULast7Days[FRdat_dep$Department=="Loire"])
plot(FRdat_dep$Date[FRdat_dep$Department=="Loire"],FRdat_dep$ICUAvg7Days[FRdat_dep$Department=="Loire"])





############################################################################
#                       REGIONAL LEVEL DATASET
############################################################################

### NEW VARIABLES: CUMDEATHS, CUMHOSP, CUMICU ###

FRdat_reg$CumDeaths <- NA
FRdat_reg$DailyDeaths[is.na(FRdat_reg$DailyDeaths)] <- 0
#loop over all regions in FR -> take cumulative sum of deaths of that region
for (i in seq_along(FR_regions$region_id)) {
  FRdat_reg$CumDeaths[FRdat_reg$Region==FR_regions$region_name[i]] <- cumsum(FRdat_reg$DailyDeaths[FRdat_reg$Region==FR_regions$region_name[i]])}

FRdat_reg$CumHosp <- NA
FRdat_reg$DailyHosp[is.na(FRdat_reg$DailyHosp)] <- 0
for (i in seq_along(FR_regions$region_id)) {
  FRdat_reg$CumHosp[FRdat_reg$Region==FR_regions$region_name[i]] <- cumsum(FRdat_reg$DailyHosp[FRdat_reg$Region==FR_regions$region_name[i]])}

FRdat_reg$CumICU <- NA
FRdat_reg$DailyICU[is.na(FRdat_reg$DailyICU)] <- 0
for (i in seq_along(FR_regions$region_id)) {
  FRdat_reg$CumICU[FRdat_reg$Region==FR_regions$region_name[i]] <- cumsum(FRdat_reg$DailyICU[FRdat_reg$Region==FR_regions$region_name[i]])}

#test regional plots
plot(FRdat_reg$Date[FRdat_reg$Region=="Bretagne"],FRdat_reg$CumDeaths[FRdat_reg$Region=="Bretagne"])
plot(FRdat_reg$Date[FRdat_reg$Region=="Normandie"],FRdat_reg$CumHosp[FRdat_reg$Region=="Normandie"])
plot(FRdat_reg$Date[FRdat_reg$Region=="Occitanie"],FRdat_reg$CumICU[FRdat_reg$Region=="Occitanie"])



### NEW VARIABLES: DEATHS/HOSP/ICU LAST7DAYS & AVG7DAYS ###

#prep regional dataset
nreg <- length(FR_regions$region_id)
add <- data.frame(Date=rep(adddate, times=nreg),Region=rep(FR_regions$region_name, each=6),
                  DailyDeaths=0,DailyHosp=0,DailyICU=0,CumDeaths=0, CumHosp=0, CumICU=0)
FRdat_reg <- rbind(FRdat_reg,add)
ndays <- length(unique(FRdat_reg$Date))
FRdat_reg <- FRdat_reg[order(FRdat_reg$Date),]
rownames(FRdat_reg) <- 1:nrow(FRdat_reg)

# Deaths
FRdat_reg$'DeathsLast7Days' <- NA
FRdat_reg$'DeathsAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    FRdat_reg$'DeathsLast7Days'[FRdat_reg$Region==FR_regions$region_name[i]][j] <-  sum(FRdat_reg$DailyDeaths[FRdat_reg$Region==FR_regions$region_name[i]][(j-6):j])
    FRdat_reg$'DeathsAvg7Days'[FRdat_reg$Region==FR_regions$region_name[i]][j] <-  mean(FRdat_reg$DailyDeaths[FRdat_reg$Region==FR_regions$region_name[i]][(j-6):j]) }}
FRdat_reg$'DeathsLast7Days'[1:(6*nreg)] <- 0
FRdat_reg$'DeathsAvg7Days'[1:(6*nreg)] <- 0

# Hosp
FRdat_reg$'HospLast7Days' <- NA
FRdat_reg$'HospAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    FRdat_reg$'HospLast7Days'[FRdat_reg$Region==FR_regions$region_name[i]][j] <-  sum(FRdat_reg$DailyHosp[FRdat_reg$Region==FR_regions$region_name[i]][(j-6):j])
    FRdat_reg$'HospAvg7Days'[FRdat_reg$Region==FR_regions$region_name[i]][j] <-  mean(FRdat_reg$DailyHosp[FRdat_reg$Region==FR_regions$region_name[i]][(j-6):j]) }}
FRdat_reg$'HospLast7Days'[1:(6*nreg)] <- 0
FRdat_reg$'HospAvg7Days'[1:(6*nreg)] <- 0

# ICU
FRdat_reg$'ICULast7Days' <- NA
FRdat_reg$'ICUAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    FRdat_reg$'ICULast7Days'[FRdat_reg$Region==FR_regions$region_name[i]][j] <-  sum(FRdat_reg$DailyICU[FRdat_reg$Region==FR_regions$region_name[i]][(j-6):j])
    FRdat_reg$'ICUAvg7Days'[FRdat_reg$Region==FR_regions$region_name[i]][j] <-  mean(FRdat_reg$DailyICU[FRdat_reg$Region==FR_regions$region_name[i]][(j-6):j]) }}
FRdat_reg$'ICULast7Days'[1:(6*nreg)] <- 0
FRdat_reg$'ICUAvg7Days'[1:(6*nreg)] <- 0

#test regional plots
plot(FRdat_reg$Date[FRdat_reg$Region=="Bretagne"],FRdat_reg$DeathsLast7Days[FRdat_reg$Region=="Bretagne"])
plot(FRdat_reg$Date[FRdat_reg$Region=="Bretagne"],FRdat_reg$DeathsAvg7Days[FRdat_reg$Region=="Bretagne"])
plot(FRdat_reg$Date[FRdat_reg$Region=="Normandie"],FRdat_reg$HospLast7Days[FRdat_reg$Region=="Normandie"])
plot(FRdat_reg$Date[FRdat_reg$Region=="Normandie"],FRdat_reg$HospAvg7Days[FRdat_reg$Region=="Normandie"])
plot(FRdat_reg$Date[FRdat_reg$Region=="Occitanie"],FRdat_reg$ICULast7Days[FRdat_reg$Region=="Occitanie"])
plot(FRdat_reg$Date[FRdat_reg$Region=="Occitanie"],FRdat_reg$ICUAvg7Days[FRdat_reg$Region=="Occitanie"])




############################################################################
#                       COUNTRY-LEVEL DATASET
############################################################################


### NEW VARIABLES: CUMCASES, CUMDEATHS, CUMHOSP, CUMICU ###

FRdat_nat$CumCases <- NA
FRdat_nat$DailyCases[is.na(FRdat_nat$DailyCases)] <- 0
FRdat_nat$CumCases <- cumsum(FRdat_nat$DailyCases)

FRdat_nat$CumDeaths <- NA
FRdat_nat$DailyDeaths[is.na(FRdat_nat$DailyDeaths)] <- 0
FRdat_nat$CumDeaths <- cumsum(FRdat_nat$DailyDeaths)

FRdat_nat$CumHosp <- NA
FRdat_nat$DailyHosp[is.na(FRdat_nat$DailyHosp)] <- 0
FRdat_nat$CumHosp <- cumsum(FRdat_nat$DailyHosp)

FRdat_nat$CumICU <- NA
FRdat_nat$DailyICU[is.na(FRdat_nat$DailyICU)] <- 0
FRdat_nat$CumICU <- cumsum(FRdat_nat$DailyICU)

#test national plots
plot(FRdat_nat$Date,FRdat_nat$CumCases)
plot(FRdat_nat$Date,FRdat_nat$CumDeaths)
plot(FRdat_nat$Date,FRdat_nat$CumHosp)
plot(FRdat_nat$Date,FRdat_nat$CumICU)


### NEW VARIABLES: DEATHS/HOSP/ICU LAST7DAYS & AVG7DAYS ###

#prep national dataset
summary(FRdat_nat$Date)   #same dates as department and regional level -> use adddate object
add <- data.frame(Date=adddate,DailyCases=0,DailyDeaths=0,DailyHosp=0,DailyICU=0,
                  CumCases=0,CumDeaths=0, CumHosp=0, CumICU=0)
FRdat_nat <- rbind(FRdat_nat,add)
ndays <- length(unique(FRdat_nat$Date))
FRdat_nat <- FRdat_nat[order(FRdat_nat$Date),]
rownames(FRdat_nat) <- 1:nrow(FRdat_nat)

# Cases
FRdat_nat$'CasesLast7Days' <- NA
FRdat_nat$'CasesAvg7Days' <- NA
for (j in 7:ndays) {
    FRdat_nat$'CasesLast7Days'[j] <-  sum(FRdat_nat$DailyCases[(j-6):j])
    FRdat_nat$'CasesAvg7Days'[j] <-  mean(FRdat_nat$DailyCases[(j-6):j]) }
FRdat_nat$'CasesLast7Days'[1:6] <- 0
FRdat_nat$'CasesAvg7Days'[1:6] <- 0

# Deaths
FRdat_nat$'DeathsLast7Days' <- NA
FRdat_nat$'DeathsAvg7Days' <- NA
for (j in 7:ndays) {
  FRdat_nat$'DeathsLast7Days'[j] <-  sum(FRdat_nat$DailyDeaths[(j-6):j])
  FRdat_nat$'DeathsAvg7Days'[j] <-  mean(FRdat_nat$DailyDeaths[(j-6):j]) }
FRdat_nat$'DeathsLast7Days'[1:6] <- 0
FRdat_nat$'DeathsAvg7Days'[1:6] <- 0

#Hosp
FRdat_nat$'HospLast7Days' <- NA
FRdat_nat$'HospAvg7Days' <- NA
for (j in 7:ndays) {
  FRdat_nat$'HospLast7Days'[j] <-  sum(FRdat_nat$DailyHosp[(j-6):j])
  FRdat_nat$'HospAvg7Days'[j] <-  mean(FRdat_nat$DailyHosp[(j-6):j]) }
FRdat_nat$'HospLast7Days'[1:6] <- 0
FRdat_nat$'HospAvg7Days'[1:6] <- 0

# ICU
FRdat_nat$'ICULast7Days' <- NA
FRdat_nat$'ICUAvg7Days' <- NA
for (j in 7:ndays) {
  FRdat_nat$'ICULast7Days'[j] <-  sum(FRdat_nat$DailyICU[(j-6):j])
  FRdat_nat$'ICUAvg7Days'[j] <-  mean(FRdat_nat$DailyICU[(j-6):j]) }
FRdat_nat$'ICULast7Days'[1:6] <- 0
FRdat_nat$'ICUAvg7Days'[1:6] <- 0

#test national plots
plot(FRdat_nat$Date,FRdat_nat$DeathsLast7Days)
plot(FRdat_nat$Date,FRdat_nat$DeathsAvg7Days)
plot(FRdat_nat$Date,FRdat_nat$CasesLast7Days)
plot(FRdat_nat$Date,FRdat_nat$CasesAvg7Days)
plot(FRdat_nat$Date,FRdat_nat$HospLast7Days)
plot(FRdat_nat$Date,FRdat_nat$HospAvg7Days)
plot(FRdat_nat$Date,FRdat_nat$ICULast7Days)
plot(FRdat_nat$Date,FRdat_nat$ICUAvg7Days)



### ADDING JOHN HOPKINS COUNTRY-LEVEL DATA ###

raw_JHcases <- read.delim("./Data/covid_data/John Hopkins_05.05.2021 - cases.txt", sep=",")
FR_JHcases <- subset(raw_JHcases,select = -c(Province.State,Lat,Long))
# aggregate per country (for france: territories + mainland)
FR_JHcases <- aggregate(FR_JHcases[,2:ncol(FR_JHcases)], by=list(FR_JHcases$Country.Region),FUN=sum)
FR_JHcases <- FR_JHcases[FR_JHcases$Group.1=="France",]
#turn into long format
FR_JHcases <- gather(FR_JHcases, key=Date, value=JHCumCases)
FR_JHcases <- FR_JHcases[-1,]
FR_JHcases$Date <- gsub("X", "",FR_JHcases$Date)
FR_JHcases$Date <- as.Date(FR_JHcases$Date, "%m.%d.%y")

raw_JHdeaths <- read.delim("./Data/covid_data/John Hopkins_05.05.2021 - deaths.txt", sep=",")
FR_JHdeaths <- subset(raw_JHdeaths,select=-c(Province.State,Lat,Long))
#aggregate per country
FR_JHdeaths <- aggregate(FR_JHdeaths[,2:ncol(FR_JHdeaths)],by=list(FR_JHdeaths$Country.Region),FUN=sum)
FR_JHdeaths <- FR_JHdeaths[FR_JHdeaths$Group.1=="France",]
#wide to long format
FR_JHdeaths <- gather(FR_JHdeaths, key=Date, value=JHCumDeaths)
FR_JHdeaths <- FR_JHdeaths[-1,]
FR_JHdeaths$Date <- gsub("X","",FR_JHdeaths$Date)
FR_JHdeaths$Date <- as.Date(FR_JHdeaths$Date,"%m.%d.%y")

#merge with national dataset, keeping all dates in JH dataset (starts earlier than nat data)
FRdat_nat <- merge(FRdat_nat, FR_JHcases,by="Date",all.y=T)
FRdat_nat <- merge(FRdat_nat, FR_JHdeaths, by="Date",all.y=T)


### CALCULATING DAILY FIGURES FOR JOHN HOPKINS DATA ###

# DailyCases JH
FRdat_nat$JHDailyCases <- c(0,diff(as.numeric(FRdat_nat$JHCumCases)))

# DailyDeaths JH
FRdat_nat$JHDailyDeaths <- c(0,diff(as.numeric(FRdat_nat$JHCumDeaths)))


### CALCULATING RELATIVE FIGURES FOR JOHN HOPKINS DATA ###

summary(FRdat_nat$Date)
#starts on 22-01-2020 -> need to add 6 days of zeros
adddate <- seq(from=as.Date("2020-01-16"), to = as.Date("2020-01-21"), by = "days")
add <- data.frame(Date=adddate,DailyCases=NA,DailyDeaths=NA,DailyHosp=NA,DailyICU=NA,
                  CumCases=NA,CumDeaths=NA, CumHosp=NA, CumICU=NA,CasesLast7Days=NA,
                  CasesAvg7Days=NA,DeathsLast7Days=NA,DeathsAvg7Days=NA,HospLast7Days=NA,
                  HospAvg7Days=NA,ICULast7Days=NA,ICUAvg7Days=NA,JHDailyCases=0,
                  JHDailyDeaths=0,JHCumCases=0,JHCumDeaths=0)
FRdat_nat <- rbind(FRdat_nat,add)
ndays <- length(unique(FRdat_nat$Date))
FRdat_nat <- FRdat_nat[order(FRdat_nat$Date),]
rownames(FRdat_nat) <- 1:nrow(FRdat_nat)
FRdat_nat$JHDailyCases <- as.integer(FRdat_nat$JHDailyCases)
FRdat_nat$JHDailyDeaths <- as.integer(FRdat_nat$JHDailyDeaths)

# Cases
FRdat_nat$'JHCasesLast7Days' <- NA
FRdat_nat$'JHCasesAvg7Days' <- NA
for (j in 7:ndays) {
  FRdat_nat$'JHCasesLast7Days'[j] <-  sum(FRdat_nat$JHDailyCases[(j-6):j])
  FRdat_nat$'JHCasesAvg7Days'[j] <-  mean(FRdat_nat$JHDailyCases[(j-6):j]) }
FRdat_nat$'JHCasesLast7Days'[1:6] <- 0
FRdat_nat$'JHCasesAvg7Days'[1:6] <- 0

# Deaths
FRdat_nat$'JHDeathsLast7Days' <- NA
FRdat_nat$'JHDeathsAvg7Days' <- NA
for (j in 7:ndays) {
  FRdat_nat$'JHDeathsLast7Days'[j] <-  sum(FRdat_nat$JHDailyDeaths[(j-6):j])
  FRdat_nat$'JHDeathsAvg7Days'[j] <-  mean(FRdat_nat$JHDailyDeaths[(j-6):j]) }
FRdat_nat$'JHDeathsLast7Days'[1:6] <- 0
FRdat_nat$'JHDeathsAvg7Days'[1:6] <- 0

#test national plots
plot(FRdat_nat$Date,FRdat_nat$JHDailyCases)
plot(FRdat_nat$Date,FRdat_nat$JHDailyDeaths)
plot(FRdat_nat$Date,FRdat_nat$JHCasesLast7Days)
plot(FRdat_nat$Date,FRdat_nat$JHCasesAvg7Days)
plot(FRdat_nat$Date,FRdat_nat$JHDeathsLast7Days)
plot(FRdat_nat$Date,FRdat_nat$JHDeathsAvg7Days)







#####################
### Save dataframes
#####################

save(FRdat_dep, file="./Data/prepared_data/FR_prepared_data-department.RData")
save(FRdat_reg, file="./Data/prepared_data/FR_prepared_data-region.RData")
save(FRdat_nat, file="./Data/prepared_data/FR_prepared_data-national.RData")
