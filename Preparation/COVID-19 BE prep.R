##############################################
###                                        ###
###    Preparing BE COVID Health Figures   ###
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
list.of.packages <- c("plyr", "tidyr","readxl")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)


####################
# Loading data
####################

#raw dataset
raw_cases <- read_xlsx("./Data/covid_data/BE Sciensano_29.04.2021.xlsx", sheet="CASES_AGESEX")
raw_hosp <- read_xlsx("./Data/covid_data/BE Sciensano_29.04.2021.xlsx", sheet="HOSP")
raw_deaths <- read_xlsx("./Data/covid_data/BE Sciensano_29.04.2021.xlsx", sheet="MORT")

#dataframe to manipulate
BEcases <- data.frame(raw_cases)
BEhosp <- data.frame(raw_hosp)
BEdeaths <- data.frame(raw_deaths)



#######################
# Inspecting data
#######################

### CASES DATASET ###
str(BEcases)
summary(BEcases)

#Convert the relevant variables to correct type
BEcases$DATE <- as.Date(BEcases$DATE)
#replace all missing values for PROVINCE with province name "UNKNOWN"
BEcases$PROVINCE[is.na(BEcases$PROVINCE)] <- "Unknown"
BEcases$PROVINCE <- factor(BEcases$PROVINCE)
levels(BEcases$PROVINCE)
#replace all missing values for REGION with region name "UNKNOWN"
BEcases$REGION[is.na(BEcases$REGION)] <- "Unknown"
BEcases$REGION <- factor(BEcases$REGION)
levels(BEcases$REGION)

#also 49 missing values for Date, together making up 94 cases
sum(BEcases$CASES[is.na(BEcases$DATE)])

#List with all provinces/regions (including UNKNOWN)
BE_provinces <- data.frame(region_id=1:length(unique(BEcases$PROVINCE)),province_name=unique(BEcases$PROVINCE))
BE_regions <- data.frame(region_id=1:length(unique(BEcases$REGION)),region_name=unique(BEcases$REGION))



### HOSPITALIZATIONS DATASET ###
str(BEhosp)
summary(BEhosp)
#Convert the relevant variables to correct type
  #here, no missing values for province or region
  #will still include UNKNOWN as a province/region level for consistency
BEhosp$DATE <- as.Date(BEhosp$DATE)
BEhosp$PROVINCE <- factor(BEhosp$PROVINCE, levels=BE_provinces$province_name)
BEhosp$REGION <- factor(BEhosp$REGION, levels=BE_regions$region_name)



### DEATHS DATASET ###
str(BEdeaths)
summary(BEdeaths)
#Convert the relevant variables to correct type
   #here, no missing values for region
   #will still include UNKNOWN as a region level for consistency
BEdeaths$DATE <- as.Date(BEdeaths$DATE)
BEdeaths$REGION <- factor(BEdeaths$REGION, levels=BE_regions$region_name)





######################################################
# CREATING PROVINCIAL, REGIONAL AND NATIONAL DATASETS
######################################################

#aggregate per province
BEcases_prov <- ddply(BEcases,.(DATE,PROVINCE),summarise, DailyCases=sum(CASES))
BEhosp_prov <- ddply(BEhosp,.(DATE,PROVINCE),summarise, TotalICU=sum(TOTAL_IN_ICU),DailyHosp=sum(NEW_IN))

#aggregate per region
BEcases_reg <- ddply(BEcases,.(DATE,REGION),summarise, DailyCases=sum(CASES))
BEhosp_reg <- ddply(BEhosp,.(DATE,REGION),summarise, TotalICU=sum(TOTAL_IN_ICU),DailyHosp=sum(NEW_IN))
BEdeaths_reg <- ddply(BEdeaths,.(DATE,REGION),summarise, DailyDeaths=sum(DEATHS))

rm(list=c('BEcases','BEhosp','BEdeaths'))

#create provincial dataset
BEdat_prov <- merge(BEcases_prov,BEhosp_prov, all=T)
BEdat_prov <- rename(BEdat_prov, replace=c("DATE"="Date","PROVINCE"="Province"))

#create regional dataset
BEdat_reg <- merge(BEcases_reg,BEhosp_reg, all=T)
BEdat_reg <- merge(BEdat_reg, BEdeaths_reg, all=T)
BEdat_reg <- rename(BEdat_reg, replace=c("DATE"="Date","REGION"="Region"))
rm(list=c('BEcases_prov','BEcases_reg','BEhosp_prov','BEhosp_reg','BEdeaths_reg'))

#create national dataset (from regional dataset -> includes number of deaths)
BEdat_nat <- ddply(BEdat_reg, .(Date), summarise, DailyCases=sum(na.omit(DailyCases)),DailyDeaths=sum(na.omit(DailyDeaths)), 
                   DailyHosp=sum(na.omit(DailyHosp)),TotalICU=sum(na.omit(TotalICU)) )



###############################################################################
#                               PROVINCIAL DATASET
###############################################################################

### INCLUDING NON OBSERVATIONS ###

#now, only observations for dates where a case or death or hospitalization was reported in the province
summary(BEdat_prov$Date)
all.dates <- seq(as.Date("2020-03-01"), as.Date("2021-04-28"), by="days")
for (i in seq_along(BE_provinces$province_name)) {
  miss.dates <- subset(all.dates, !(all.dates %in% BEdat_prov$Date[BEdat_prov$Province==BE_provinces$province_name[i]]))
  miss.dates <- as.Date(miss.dates,format="%Y-%m-%d")
  if (length(miss.dates) != 0) {
    add <- cbind(as.character(miss.dates), as.character(BE_provinces$province_name[i]),NA,NA,NA)
    colnames(add) <- c("Date", "Province","DailyCases", "TotalICU","DailyHosp")
    BEdat_prov <- rbind(BEdat_prov, add)} }
BEdat_prov <- BEdat_prov[order(BEdat_prov$Date, BEdat_prov$Province),]
rownames(BEdat_prov) <- seq(length=nrow(BEdat_prov))
#Cases/ICU/Hosp have turned into characters
BEdat_prov$DailyCases <- as.integer(BEdat_prov$DailyCases)
BEdat_prov$TotalICU <- as.integer(BEdat_prov$TotalICU)
BEdat_prov$DailyHosp <- as.integer(BEdat_prov$DailyHosp)

#test plots for different provinces
plot(BEdat_prov$Date[BEdat_prov$Province=="Limburg"],BEdat_prov$DailyCases[BEdat_prov$Province=="Limburg"])
plot(BEdat_prov$Date[BEdat_prov$Province=="Brussels"],BEdat_prov$DailyHosp[BEdat_prov$Province=="Brussels"])



### CALCULATING CUMULATIVE VARIABLES ###

BEdat_prov$CumCases <- NA
BEdat_prov$CumHosp <- NA

# Cases
BEdat_prov$DailyCases[is.na(BEdat_prov$DailyCases)] <- 0
for (i in seq_along(BE_provinces$region_id)) {
  BEdat_prov$CumCases[BEdat_prov$Province==BE_provinces$province_name[i]] <- cumsum(BEdat_prov$DailyCases[BEdat_prov$Province==BE_provinces$province_name[i]]) }

# Hosp
BEdat_prov$DailyHosp[is.na(BEdat_prov$DailyHosp)] <- 0
for (i in seq_along(BE_provinces$region_id)) {
  BEdat_prov$CumHosp[BEdat_prov$Province==BE_provinces$province_name[i]] <- cumsum(BEdat_prov$DailyHosp[BEdat_prov$Province==BE_provinces$province_name[i]])}

#test plots for different provinces
plot(BEdat_prov$Date[BEdat_prov$Province=="Antwerpen"],BEdat_prov$CumCases[BEdat_prov$Province=="Antwerpen"])
plot(BEdat_prov$Date[BEdat_prov$Province=="Liège"],BEdat_prov$CumHosp[BEdat_prov$Province=="Liège"])



### CALCULATING RELATIVE VARIABLES ###

summary(BEdat_prov)
#dataset starts on 01-03-2020 -> need to add 6 days of zeros, for each province
adddate <- seq(from=as.Date("2020-02-24"), to = as.Date("2020-02-29"), by = "days")
nprov <- length(BE_provinces$region_id)
add <- data.frame(Date=rep(adddate, times=nprov),Province=rep(BE_provinces$province_name, each=6),
                  DailyCases=0,TotalICU=0,DailyHosp=0,CumCases=0, CumHosp=0)
BEdat_prov <- rbind(BEdat_prov,add)
ndays <- length(unique(BEdat_prov$Date))
BEdat_prov <- BEdat_prov[order(BEdat_prov$Date),]
rownames(BEdat_prov) <- 1:nrow(BEdat_prov)

# Cases
BEdat_prov$'CasesLast7Days' <- NA
BEdat_prov$'CasesAvg7Days' <- NA
for (i in 1:nprov) {
  for (j in 7:ndays) {
    BEdat_prov$'CasesLast7Days'[BEdat_prov$Province==BE_provinces$province_name[i]][j] <-  sum(BEdat_prov$DailyCases[BEdat_prov$Province==BE_provinces$province_name[i]][(j-6):j])
    BEdat_prov$'CasesAvg7Days'[BEdat_prov$Province==BE_provinces$province_name[i]][j] <-  mean(BEdat_prov$DailyCases[BEdat_prov$Province==BE_provinces$province_name[i]][(j-6):j]) }}
BEdat_prov$'CasesLast7Days'[1:(6*nprov)] <- 0
BEdat_prov$'CasesAvg7Days'[1:(6*nprov)] <- 0

# Hosp
BEdat_prov$'HospLast7Days' <- NA
BEdat_prov$'HospAvg7Days' <- NA
for (i in 1:nprov) {
  for (j in 7:ndays) {
    BEdat_prov$'HospLast7Days'[BEdat_prov$Province==BE_provinces$province_name[i]][j] <-  sum(BEdat_prov$DailyHosp[BEdat_prov$Province==BE_provinces$province_name[i]][(j-6):j])
    BEdat_prov$'HospAvg7Days'[BEdat_prov$Province==BE_provinces$province_name[i]][j] <-  mean(BEdat_prov$DailyHosp[BEdat_prov$Province==BE_provinces$province_name[i]][(j-6):j]) }}
BEdat_prov$'HospLast7Days'[1:(6*nprov)] <- 0
BEdat_prov$'HospAvg7Days'[1:(6*nprov)] <- 0

# test plots for different provinces
plot(BEdat_prov$Date[BEdat_prov$Province=="Antwerpen"], BEdat_prov$CasesAvg7Days[BEdat_prov$Province=="Antwerpen"])
plot(BEdat_prov$Date[BEdat_prov$Province=="Antwerpen"], BEdat_prov$CasesLast7Days[BEdat_prov$Province=="Antwerpen"])
plot(BEdat_prov$Date[BEdat_prov$Province=="Liège"], BEdat_prov$HospLast7Days[BEdat_prov$Province=="Liège"])
plot(BEdat_prov$Date[BEdat_prov$Province=="Liège"], BEdat_prov$HospAvg7Days[BEdat_prov$Province=="Liège"])





###############################################################################
#                               REGIONAL DATASET 
###############################################################################

### INCLUDING NON OBSERVATIONS ###

#now, only observations for dates where a case or death or hospitalization was reported in the region
summary(BEdat_reg$Date)
all.dates <- seq(as.Date("2020-03-01"), as.Date("2021-04-28"), by="days")
for (i in seq_along(BE_regions$region_name)) {
  miss.dates <- subset(all.dates, !(all.dates %in% BEdat_reg$Date[BEdat_reg$Region==BE_regions$region_name[i]]))
  miss.dates <- as.Date(miss.dates,format="%Y-%m-%d")
  if (length(miss.dates) != 0) {
    add <- cbind(as.character(miss.dates), as.character(BE_regions$region_name[i]),NA,NA,NA,NA)
    colnames(add) <- c("Date", "Region","DailyCases", "TotalICU","DailyHosp","DailyDeaths")
    BEdat_reg <- rbind(BEdat_reg, add)} }
BEdat_reg <- BEdat_reg[order(BEdat_reg$Date, BEdat_reg$Region),]
rownames(BEdat_reg) <- seq(length=nrow(BEdat_reg))
#Cases/ICU/Hosp have turned into characters
BEdat_reg$DailyCases <- as.integer(BEdat_reg$DailyCases)
BEdat_reg$TotalICU <- as.integer(BEdat_reg$TotalICU)
BEdat_reg$DailyHosp <- as.integer(BEdat_reg$DailyHosp)
BEdat_reg$DailyDeaths <- as.integer(BEdat_reg$DailyDeaths)

#test plots for different Regions
plot(BEdat_reg$Date[BEdat_reg$Region=="Flanders"],BEdat_reg$DailyCases[BEdat_reg$Region=="Flanders"])
plot(BEdat_reg$Date[BEdat_reg$Region=="Brussels"],BEdat_reg$DailyHosp[BEdat_reg$Region=="Brussels"])
plot(BEdat_reg$Date[BEdat_reg$Region=="Wallonia"],BEdat_reg$DailyDeaths[BEdat_reg$Region=="Wallonia"])



### CALCULATING CUMULATIVE VARIABLES ###

BEdat_reg$CumCases <- NA
BEdat_reg$CumDeaths <- NA
BEdat_reg$CumHosp <- NA

# Cases
BEdat_reg$DailyCases[is.na(BEdat_reg$DailyCases)] <- 0
for (i in seq_along(BE_regions$region_id)) {
  BEdat_reg$CumCases[BEdat_reg$Region==BE_regions$region_name[i]] <- cumsum(BEdat_reg$DailyCases[BEdat_reg$Region==BE_regions$region_name[i]]) }

# Deaths
BEdat_reg$DailyDeaths[is.na(BEdat_reg$DailyDeaths)] <- 0
for (i in seq_along(BE_regions$region_id)) {
  BEdat_reg$CumDeaths[BEdat_reg$Region==BE_regions$region_name[i]] <- cumsum(BEdat_reg$DailyDeaths[BEdat_reg$Region==BE_regions$region_name[i]])}

# Hosp
BEdat_reg$DailyHosp[is.na(BEdat_reg$DailyHosp)] <- 0
for (i in seq_along(BE_regions$region_id)) {
  BEdat_reg$CumHosp[BEdat_reg$Region==BE_regions$region_name[i]] <- cumsum(BEdat_reg$DailyHosp[BEdat_reg$Region==BE_regions$region_name[i]])}

#test plots for different regions
plot(BEdat_reg$Date[BEdat_reg$Region=="Flanders"],BEdat_reg$CumCases[BEdat_reg$Region=="Flanders"])
plot(BEdat_reg$Date[BEdat_reg$Region=="Wallonia"],BEdat_reg$CumHosp[BEdat_reg$Region=="Wallonia"])
plot(BEdat_reg$Date[BEdat_reg$Region=="Brussels"],BEdat_reg$CumDeaths[BEdat_reg$Region=="Brussels"])




### CALCULATING RELATIVE VARIABLES ###

summary(BEdat_reg)
#dataset starts on 01-03-2020 -> need to add 6 days of zeros, for each region
nreg <- length(BE_regions$region_id)
add <- data.frame(Date=rep(adddate, times=nprov),Region=rep(BE_regions$region_name, each=6),
                  DailyCases=0,TotalICU=0,DailyHosp=0,DailyDeaths=0,CumCases=0, CumDeaths=0, CumHosp=0)
BEdat_reg <- rbind(BEdat_reg,add)
BEdat_reg <- BEdat_reg[order(BEdat_reg$Date),]
rownames(BEdat_reg) <- 1:nrow(BEdat_reg)

# Cases
BEdat_reg$'CasesLast7Days' <- NA
BEdat_reg$'CasesAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    BEdat_reg$'CasesLast7Days'[BEdat_reg$Region==BE_regions$region_name[i]][j] <-  sum(BEdat_reg$DailyCases[BEdat_reg$Region==BE_regions$region_name[i]][(j-6):j])
    BEdat_reg$'CasesAvg7Days'[BEdat_reg$Region==BE_regions$region_name[i]][j] <-  mean(BEdat_reg$DailyCases[BEdat_reg$Region==BE_regions$region_name[i]][(j-6):j]) }}
BEdat_reg$'CasesLast7Days'[1:(6*nreg)] <- 0
BEdat_reg$'CasesAvg7Days'[1:(6*nreg)] <- 0

# Deaths
BEdat_reg$'DeathsLast7Days' <- NA
BEdat_reg$'DeathsAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    BEdat_reg$'DeathsLast7Days'[BEdat_reg$Region==BE_regions$region_name[i]][j] <-  sum(BEdat_reg$DailyDeaths[BEdat_reg$Region==BE_regions$region_name[i]][(j-6):j])
    BEdat_reg$'DeathsAvg7Days'[BEdat_reg$Region==BE_regions$region_name[i]][j] <-  mean(BEdat_reg$DailyDeaths[BEdat_reg$Region==BE_regions$region_name[i]][(j-6):j]) }}
BEdat_reg$'DeathsLast7Days'[1:(6*nreg)] <- 0
BEdat_reg$'DeathsAvg7Days'[1:(6*nreg)] <- 0

# Hosp
BEdat_reg$'HospLast7Days' <- NA
BEdat_reg$'HospAvg7Days' <- NA
for (i in 1:nreg) {
  for (j in 7:ndays) {
    BEdat_reg$'HospLast7Days'[BEdat_reg$Region==BE_regions$region_name[i]][j] <-  sum(BEdat_reg$DailyHosp[BEdat_reg$Region==BE_regions$region_name[i]][(j-6):j])
    BEdat_reg$'HospAvg7Days'[BEdat_reg$Region==BE_regions$region_name[i]][j] <-  mean(BEdat_reg$DailyHosp[BEdat_reg$Region==BE_regions$region_name[i]][(j-6):j]) }}
BEdat_reg$'HospLast7Days'[1:(6*nreg)] <- 0
BEdat_reg$'HospAvg7Days'[1:(6*nreg)] <- 0

# test plots for different regions
plot(BEdat_reg$Date[BEdat_reg$Region=="Brussels"], BEdat_reg$CasesAvg7Days[BEdat_reg$Region=="Brussels"])
plot(BEdat_reg$Date[BEdat_reg$Region=="Brussels"], BEdat_reg$CasesLast7Days[BEdat_reg$Region=="Brussels"])
plot(BEdat_reg$Date[BEdat_reg$Region=="Flanders"], BEdat_reg$HospLast7Days[BEdat_reg$Region=="Flanders"])
plot(BEdat_reg$Date[BEdat_reg$Region=="Flanders"], BEdat_reg$HospAvg7Days[BEdat_reg$Region=="Flanders"])
plot(BEdat_reg$Date[BEdat_reg$Region=="Wallonia"], BEdat_reg$DeathsLast7Days[BEdat_reg$Region=="Wallonia"])
plot(BEdat_reg$Date[BEdat_reg$Region=="Wallonia"], BEdat_reg$DeathsAvg7Days[BEdat_reg$Region=="Wallonia"])






###############################################################################
#                               NATIONAL DATASET 
###############################################################################

### CREATE CUMULATIVE VARIABLES ###

BEdat_nat$CumCases <- NA
BEdat_nat$DailyCases[is.na(BEdat_nat$DailyCases)] <- 0
BEdat_nat$CumCases <- cumsum(BEdat_nat$DailyCases)

BEdat_nat$CumDeaths <- NA
BEdat_nat$DailyDeaths[is.na(BEdat_nat$DailyDeaths)] <- 0
BEdat_nat$CumDeaths <- cumsum(BEdat_nat$DailyDeaths)

BEdat_nat$CumHosp <- NA
BEdat_nat$DailyHosp[is.na(BEdat_nat$DailyHosp)] <- 0
BEdat_nat$CumHosp <- cumsum(BEdat_nat$DailyHosp)

#test national plots
plot(BEdat_nat$Date,BEdat_nat$CumCases)
plot(BEdat_nat$Date,BEdat_nat$CumDeaths)
plot(BEdat_nat$Date,BEdat_nat$CumHosp)



### CREATE RELATIVE VARIABLES ###

#prep national dataset
summary(BEdat_nat$Date)   #same dates as provincial and regional level -> use adddate object
add <- data.frame(Date=adddate,DailyCases=0,DailyDeaths=0,DailyHosp=0,TotalICU=0,CumCases=0,CumDeaths=0, CumHosp=0)
BEdat_nat <- rbind(BEdat_nat,add)
ndays <- length(unique(BEdat_nat$Date))
BEdat_nat <- BEdat_nat[order(BEdat_nat$Date),]
rownames(BEdat_nat) <- 1:nrow(BEdat_nat)

# Cases
BEdat_nat$'CasesLast7Days' <- NA
BEdat_nat$'CasesAvg7Days' <- NA
for (j in 7:ndays) {
  BEdat_nat$'CasesLast7Days'[j] <-  sum(BEdat_nat$DailyCases[(j-6):j])
  BEdat_nat$'CasesAvg7Days'[j] <-  mean(BEdat_nat$DailyCases[(j-6):j]) }
BEdat_nat$'CasesLast7Days'[1:6] <- 0
BEdat_nat$'CasesAvg7Days'[1:6] <- 0

# Deaths
BEdat_nat$'DeathsLast7Days' <- NA
BEdat_nat$'DeathsAvg7Days' <- NA
for (j in 7:ndays) {
  BEdat_nat$'DeathsLast7Days'[j] <-  sum(BEdat_nat$DailyDeaths[(j-6):j])
  BEdat_nat$'DeathsAvg7Days'[j] <-  mean(BEdat_nat$DailyDeaths[(j-6):j]) }
BEdat_nat$'DeathsLast7Days'[1:6] <- 0
BEdat_nat$'DeathsAvg7Days'[1:6] <- 0

#Hosp
BEdat_nat$'HospLast7Days' <- NA
BEdat_nat$'HospAvg7Days' <- NA
for (j in 7:ndays) {
  BEdat_nat$'HospLast7Days'[j] <-  sum(BEdat_nat$DailyHosp[(j-6):j])
  BEdat_nat$'HospAvg7Days'[j] <-  mean(BEdat_nat$DailyHosp[(j-6):j]) }
BEdat_nat$'HospLast7Days'[1:6] <- 0
BEdat_nat$'HospAvg7Days'[1:6] <- 0

#test national plots
plot(BEdat_nat$Date,BEdat_nat$DeathsLast7Days)
plot(BEdat_nat$Date,BEdat_nat$DeathsAvg7Days)
plot(BEdat_nat$Date,BEdat_nat$CasesLast7Days)
plot(BEdat_nat$Date,BEdat_nat$CasesAvg7Days)
plot(BEdat_nat$Date,BEdat_nat$HospLast7Days)
plot(BEdat_nat$Date,BEdat_nat$HospAvg7Days)




### ADDING JOHN HOPKINS COUNTRY-LEVEL DATA ###

raw_JHcases <- read.delim("./Data/covid_data/John Hopkins_05.05.2021 - cases.txt", sep=",")
BE_JHcases <- subset(raw_JHcases,select = -c(Province.State,Lat,Long))
BE_JHcases <- BE_JHcases[BE_JHcases$Country.Region =="Belgium",]
#turn into long format
BE_JHcases <- gather(BE_JHcases, key=Date, value=JHCumCases)
BE_JHcases <- BE_JHcases[-1,]
BE_JHcases$Date <- gsub("X", "",BE_JHcases$Date)
BE_JHcases$Date <- as.Date(BE_JHcases$Date, "%m.%d.%y")

raw_JHdeaths <- read.delim("./Data/covid_data/John Hopkins_05.05.2021 - deaths.txt", sep=",")
BE_JHdeaths <- subset(raw_JHdeaths,select = -c(Province.State,Lat,Long))
BE_JHdeaths <- BE_JHdeaths[BE_JHdeaths$Country.Region =="Belgium",]
#turn into long format
BE_JHdeaths <- gather(BE_JHdeaths, key=Date, value=JHCumDeaths)
BE_JHdeaths <- BE_JHdeaths[-1,]
BE_JHdeaths$Date <- gsub("X", "",BE_JHdeaths$Date)
BE_JHdeaths$Date <- as.Date(BE_JHdeaths$Date, "%m.%d.%y")

#merge with national dataset, keeping all dates in JH dataset (starts earlier than nat data)
BEdat_nat <- merge(BEdat_nat, BE_JHcases,by="Date",all.y=T)
BEdat_nat <- merge(BEdat_nat, BE_JHdeaths, by="Date",all.y=T)


### CALCULATING DAILY FIGURES FOR JOHN HOPKINS DATA ###

# DailyCases JH
BEdat_nat$JHDailyCases <- c(0,diff(as.numeric(BEdat_nat$JHCumCases)))

# DailyDeaths JH
BEdat_nat$JHDailyDeaths <- c(0,diff(as.numeric(BEdat_nat$JHCumDeaths)))


### CALCULATING RELATIVE FIGURES FOR JOHN HOPKINS DATA ###

summary(BEdat_nat$Date)
#starts on 22-01-2020 -> need to add 6 days of zeros
adddate <- seq(from=as.Date("2020-01-16"), to = as.Date("2020-01-21"), by = "days")
add <- data.frame(Date=adddate,DailyCases=NA,DailyDeaths=NA,DailyHosp=NA,TotalICU=NA,
                  CumCases=NA,CumDeaths=NA, CumHosp=NA,CasesLast7Days=NA,
                  CasesAvg7Days=NA,DeathsLast7Days=NA,DeathsAvg7Days=NA,HospLast7Days=NA,
                  HospAvg7Days=NA,JHDailyCases=0,JHDailyDeaths=0,JHCumCases=0,JHCumDeaths=0)
BEdat_nat <- rbind(BEdat_nat,add)
ndays <- length(unique(BEdat_nat$Date))
BEdat_nat <- BEdat_nat[order(BEdat_nat$Date),]
rownames(BEdat_nat) <- 1:nrow(BEdat_nat)
BEdat_nat$JHDailyCases <- as.integer(BEdat_nat$JHDailyCases)
BEdat_nat$JHDailyDeaths <- as.integer(BEdat_nat$JHDailyDeaths)

# Cases
BEdat_nat$'JHCasesLast7Days' <- NA
BEdat_nat$'JHCasesAvg7Days' <- NA
for (j in 7:ndays) {
  BEdat_nat$'JHCasesLast7Days'[j] <-  sum(BEdat_nat$JHDailyCases[(j-6):j])
  BEdat_nat$'JHCasesAvg7Days'[j] <-  mean(BEdat_nat$JHDailyCases[(j-6):j]) }
BEdat_nat$'JHCasesLast7Days'[1:6] <- 0
BEdat_nat$'JHCasesAvg7Days'[1:6] <- 0

# Deaths
BEdat_nat$'JHDeathsLast7Days' <- NA
BEdat_nat$'JHDeathsAvg7Days' <- NA
for (j in 7:ndays) {
  BEdat_nat$'JHDeathsLast7Days'[j] <-  sum(BEdat_nat$JHDailyDeaths[(j-6):j])
  BEdat_nat$'JHDeathsAvg7Days'[j] <-  mean(BEdat_nat$JHDailyDeaths[(j-6):j]) }
BEdat_nat$'JHDeathsLast7Days'[1:6] <- 0
BEdat_nat$'JHDeathsAvg7Days'[1:6] <- 0

#test national plots
plot(BEdat_nat$Date,BEdat_nat$JHDailyCases)
plot(BEdat_nat$Date,BEdat_nat$JHDailyDeaths)
plot(BEdat_nat$Date,BEdat_nat$JHCasesLast7Days)
plot(BEdat_nat$Date,BEdat_nat$JHCasesAvg7Days)
plot(BEdat_nat$Date,BEdat_nat$JHDeathsLast7Days)
plot(BEdat_nat$Date,BEdat_nat$JHDeathsAvg7Days)







#############################
### Save BEdat dataframes
#############################

save(BEdat_prov, file="./Data/prepared_data/BE_province_prepared_data.RData")
save(BEdat_reg, file="./Data/prepared_data/BE_region_prepared_data.RData")
save(BEdat_nat, file="./Data/prepared_data/BE_national_prepared_data.RData")
