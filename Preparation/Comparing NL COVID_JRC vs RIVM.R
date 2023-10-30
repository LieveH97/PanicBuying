############################################################
###                                                      ###
###    Comparing NL COVID Health Figures: RIVM vs. JRC   ###
###                                                      ###
############################################################


rm(list=ls())

### Relative path setting - adapting to the operating computer
getwd()
if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
  project_path <- "C:/Work at KU Leuven/Projects/RetailCOVID19"
}else if (Sys.info()["nodename"] == "LAPTOP-KBL77S8J"){
  project_path <- "C:/Users/lieve/Documents/GitHub/RetailCOVID19"
}else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
  project_path <- "C:/PhD KU Leuven/Projects_Github/RetailCOVID19"
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

#loading NL data from national institute RIVM
load("./R_Files/Preparation/NLdataRIVM.RData")
RIVMraw <- NLagg     #raw data
rm(NLagg)
RIVMdat <- RIVMraw      #data to manipulate

#loading data from JRC
load("./R_Files/Preparation/dataJRC.RData")
JRCraw <- sub_health   #raw data
rm(sub_health)
JRCdat <- JRCraw       #data to manipulate





###############################
# Transforming data to match
###############################


### TRANSFORMING JRC DATA ###

JRCdat <- JRCdat[JRCdat$CountryName=="Netherlands",]   #only NL data
JRCdat$Region <- factor(JRCdat$Region)                 #only keep NL regions as factors
JRCdat <- subset(JRCdat, select= -c(iso3,CountryName,lat,lon,EUcountry,EUCPMcountry,NUTS,
                                    DailyPositive, DailyDeath, DailyRecovered, CumulativeRecovered,
                                    CurrentlyPositive, IntensiveCare, DailyRecovFixed)) #exclude unused variables
 #renaming variables
JRCdat$Province <- JRCdat$Region              
JRCdat$DailyCases <- JRCdat$DailyPosFix
JRCdat$DailyDeath <- JRCdat$DailyDeathFix
JRCdat$CumCases <- JRCdat$CumulativePositive
JRCdat$CumDeaths <- JRCdat$CumulativeDeceased
JRCdat$CumHospitalized <- JRCdat$Hospitalized
JRCdat <- subset(JRCdat, select= -c(DailyPosFix, DailyDeathFix,CumulativePositive, CumulativeDeceased,
                                    Hospitalized, Region)) #exclude variables
#creating a daily hospitalized variable
JRCdat$DailyHospitalized <- NA
NL_regions <- data.frame(region_id=1:length(unique(JRCdat$Province)),region_name=unique(JRCdat$Province))
for (i in seq_along(NL_regions$region_id)) {
  JRCdat$DailyHospitalized[JRCdat$Province==NL_regions$region_name[i]] <- c(0, diff(JRCdat$CumHospitalized[JRCdat$Province==NL_regions$region_name[i]]))
}
JRCdat <- JRCdat[,c(1:4,8,5:7)]





### TRANSFORMING RIVM DATA ###

#dropping observations after 2020-09-21 in RIVM data
excl.dates <- seq(as.Date("2020-09-22"), as.Date("2020-10-15"), by="days")
RIVMdat <- subset(RIVMdat, !(RIVMdat$Date %in% excl.dates))
rm(excl.dates)
#translate Province names in RIVM data to English
levels(RIVMdat$Province) <- revalue(RIVMdat$Province, c("Noord-Brabant"="North Brabant","Noord-Holland"="North Holland", 
                            "Zuid-Holland"="South Holland"))
RIVMdat$Province <- factor(RIVMdat$Province, levels(RIVMdat$Province)[c(1:9,12,10,11)])
levels(RIVMdat$Province)
levels(JRCdat$Province)
RIVMdat <- RIVMdat[order(RIVMdat$Date, RIVMdat$Province),]   #order according to new factor order of province
rownames(RIVMdat) <- seq(length=nrow(RIVMdat))




### INCLUDING NON-OBSERVATIONS IN JRC DATA ###

all.dates <- seq(as.Date("2020-02-27",format="%Y-%m-%d"), as.Date("2020-09-21",format="%Y-%m-%d"), by="days")
#loop over provinces
#create new rows with missing date + province name
for (i in seq_along(NL_regions$region_name)) {
  miss.dates <- subset(all.dates, !(all.dates %in% JRCdat$Date[JRCdat$Province==NL_regions$region_name[i]]))
  miss.dates <- as.Date(miss.dates,format="%Y-%m-%d")
  add <- cbind(as.character(miss.dates), as.character(NL_regions$region_name[i]), NA,NA,NA,NA,NA,NA)
  colnames(add) <- c("Date", "Province","DailyCases", "DailyDeath",  "DailyHospitalized", "CumCases", "CumDeaths", "CumHospitalized")
  JRCdat <- rbind(JRCdat, add)
  }
JRCdat <- JRCdat[order(JRCdat$Date, JRCdat$Province),]   #order in the same way as RIVM data
rownames(JRCdat) <- seq(length=nrow(JRCdat))


#fill new rows with 0's for Daily variables
JRCdat$DailyCases[is.na(JRCdat$DailyCases)] <- 0
JRCdat$DailyDeath[is.na(JRCdat$DailyDeath)] <- 0
JRCdat$DailyHospitalized[is.na(JRCdat$DailyHospitalized)] <- 0

#fill new rows with previous values for Cum variables
JRCdat <- JRCdat[order(JRCdat$Province),]       #order by province (needed for for-loop)
rownames(JRCdat) <- seq(length=nrow(JRCdat))
#CumCases
JRCdat$CumCases[1] <- 0                         #otherwise error, first observation in ordered dataset is NA
for (i in seq_along(JRCdat$Date)) {
  if (is.na(JRCdat$CumCases[i])) {
    JRCdat$CumCases[i] <- ifelse(JRCdat[i-1,]$Province == JRCdat[i,]$Province,JRCdat$CumCases[i-1],0)
  }
}
#CumDeath
JRCdat$CumDeaths[1] <- 0                         #otherwise error, first observation in ordered dataset is NA
for (i in seq_along(JRCdat$Date)) {
  if (is.na(JRCdat$CumDeaths[i])) {
    JRCdat$CumDeaths[i] <- ifelse(JRCdat[i-1,]$Province == JRCdat[i,]$Province,JRCdat$CumDeaths[i-1],0)
  }
}
#CumHospitalized
JRCdat$CumHospitalized[1] <- 0                         #otherwise error, first observation in ordered dataset is NA
for (i in seq_along(JRCdat$Date)) {
  if (is.na(JRCdat$CumHospitalized[i])) {
    JRCdat$CumHospitalized[i] <- ifelse(JRCdat[i-1,]$Province == JRCdat[i,]$Province,JRCdat$CumHospitalized[i-1],0)
  }
}
JRCdat <- JRCdat[order(JRCdat$Date, JRCdat$Province),]   #order in the same way as RIVM data
rownames(JRCdat) <- seq(length=nrow(JRCdat))
JRCdat$DailyCases <- as.integer(JRCdat$DailyCases)       #data type changed to character
JRCdat$DailyDeath <- as.integer(JRCdat$DailyDeath)
JRCdat$DailyHospitalized <- as.integer(JRCdat$DailyHospitalized)
JRCdat$CumCases <- as.integer(JRCdat$CumCases)
JRCdat$CumDeaths <- as.integer(JRCdat$CumDeaths)
JRCdat$CumHospitalized <- as.integer(JRCdat$CumHospitalized)






###############################
# Calculating Residuals
###############################

resNL <- data.frame(Date=RIVMdat$Date,Province=RIVMdat$Province)
for (i in seq_along(resNL$Date)) {
  resNL$resDailyCases[i] <- RIVMdat$DailyCases[i] - JRCdat$DailyCases[i]
  resNL$resDailyDeaths[i] <- RIVMdat$DailyDeath[i] - JRCdat$DailyDeath[i]
  resNL$resDailyHospitalized[i] <- RIVMdat$DailyHospitalized[i] - JRCdat$DailyHospitalized[i]
  resNL$resCumCases[i] <- RIVMdat$CumCases[i] - JRCdat$CumCases[i]
  resNL$resCumDeaths[i] <- RIVMdat$CumDeaths[i] - JRCdat$CumDeaths[i]
  resNL$resCumHospitalized[i] <- RIVMdat$CumHospitalized[i] - JRCdat$CumHospitalized[i]
  
}

summary(resNL)

write.csv(resNL, file="./R_Files/Preparation/residualsNL.csv",row.names = F)





###########################
# Plotting Residuals
###########################

#if residual > 0: JRCdata are smaller than national data


#Daily Positive Cases
plot(resNL$resDailyCases)
sum(resNL$resDailyCases != 0)

#Daily Deaths
plot(resNL$resDailyDeaths)      #deaths only reported in JRC data from July 18 onwards
sum(resNL$resDailyDeaths != 0)

#Daily Hospitalizations
plot(resNL$resDailyHospitalized) #hospitalizations only reported in JRC data from July 18 onwards
sum(resNL$resDailyHospitalized != 0)

