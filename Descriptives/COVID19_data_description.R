#########################################
###                                   ###
###     COVID-19 DATA DESCRIPTION     ###
###                                   ###
#########################################

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
list.of.packages <- c("ggplot2", "plyr")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)





####################
# LOADING NL DATA
####################

load(file="./Data/prepared_data/NL_prepared_data.RData")
NLraw <- NLdat
NLdat$Country <- "Netherlands"



####################
# LOADING DE DATA
####################


load(file="./Data/prepared_data/DE_prepared_data.RData")
DEraw <- DEdat
DEdat$Country <- "Germany"



####################
# LOADING BE DATA
####################

load(file="./Data/prepared_data/BE_region_prepared_data.RData")
BEraw <- BEagg_reg
BEraw$DailyCases <- as.double(BEraw$DailyCases)

#renaming columns so they match other countries
names(BEraw)[names(BEraw)=="REGION"] <- "Province"
names(BEraw)[names(BEraw)=="DATE"] <- "Date"
levels(BEraw$Province)[levels(BEraw$Province)=="Unknown"] <- "Unknown_BE"
BEraw$Country <- "Belgium"

#only keep columns Date - Province - Cases
keeps <- c("Date","Province","DailyCases","CumCases", "Country")
BEdat <- BEraw[,names(BEraw) %in% keeps]
summary(BEdat)

#BE data starts at 2020-03-01
#add 6 days before with zeros
nprov <- length(levels(BEdat$Province))
BEadd <- data.frame(Date=rep(seq(as.Date("2020-02-24"), as.Date("2020-02-29"), by="days"), nprov),
                    Province=rep(levels(BEdat$Province), each=6), DailyCases = rep(0,6*nprov), 
                    CumCases = rep(0,6*nprov), Country=rep("Belgium", nprov*6))
BEdat <- rbind(BEadd,BEdat)
BEdat$Province <- factor(BEdat$Province)

#create a variable with the number of new cases in last 7 days
#create a variable with the average number of new cases (calculated over last 7 days)
ndays_BE <- length(unique(BEdat$Date))
days_BE <- unique(BEdat$Date)
prov <- levels(BEdat$Province)
nprov <- length(prov)
BEdat$'Last7Days' <- NA
BEdat$'Avg7Days' <- NA
for (i in seq_along(prov)) {
  for (j in 7:ndays_BE) {
    BEdat$'Last7Days'[BEdat$Province==prov[i]][j] <-  sum(BEdat$DailyCases[BEdat$Province==prov[i]][(j-6):j])
    BEdat$'Avg7Days'[BEdat$Province==prov[i]][j] <-  mean(BEdat$DailyCases[BEdat$Province==prov[i]][(j-6):j])
  }
}
BEdat$'Last7Days'[1:(6*nprov)] <- 0
BEdat$'Avg7Days'[1:(6*nprov)] <- 0
#rows with missing date: zeros
BEdat$'Last7Days'[is.na(BEdat$Date)] <- 0
BEdat$'Avg7Days'[is.na(BEdat$Date)] <- 0


rm(BEagg_reg,BEadd,keeps, nprov, prov, days_BE)






#######################################
# BINDING THE DATA INTO ONE DATAFRAME
#######################################

#columns have same names + date is same format
dat <- rbind(NLdat,DEdat,BEdat)
#make country a factor and make it the third column (after Date and Province)
dat$Country <- factor(dat$Country)
dat <- dat[,c(1:2,5,3:4,6:7)]
#reorder the province levels so they're alphabetical
dat$Province <- as.character(dat$Province)
dat$Province <- factor(dat$Province)
#sort the data on date and province name
dat <- dat[order(dat$Date,dat$Province, dat$Country),]
rownames(dat) <- seq(length=nrow(dat))
rm(BEdat, NLdat, DEdat)



###############################
# ADDING THE POPULATION DATA 
###############################

load(file="./Data/prepared_data/2020_population_figures.RData")
population$Province <- gsub("ü", replacement="ue",population$Province)

dat <- merge(dat,population,by="Province")
#sort the data on date and province name
dat <- dat[order(dat$Date,dat$Province, dat$Country),]
rownames(dat) <- seq(length=nrow(dat))
rm(population)

dat$DailyCasesPer100000 <- dat$DailyCases/dat$Population * 100000
dat$Last7DaysPer100000 <- dat$Last7Days/dat$Population * 100000
dat$Avg7DaysPer100000 <- dat$Avg7Days/dat$Population * 100000




#####################################
# CREATING A COUNTRY-LEVEL DATASET 
#####################################

#using ddply, it becomes hard to calculate the "7Days" variables, since different countries start reporting on different dates
#so use ddply, then split up per country, calculate "7Days" variables, then merge
dat_country <- ddply(dat, .(Country, Date), summarize, DailyCases=sum(DailyCases), CumCases=sum(CumCases), Population=sum(unique(Population)))

# BE
dat_countryBE <- dat_country[dat_country$Country=="Belgium",]
dat_countryBE$'Last7Days' <- NA
dat_countryBE$'Avg7Days' <- NA
for (j in 7:ndays_BE) {
  dat_countryBE$'Last7Days'[j] <-  sum(dat_countryBE$DailyCases[(j-6):j])
  dat_countryBE$'Avg7Days'[j] <-  mean(dat_countryBE$DailyCases[(j-6):j])
 }
dat_countryBE$'Last7Days'[1:6] <- 0
dat_countryBE$'Avg7Days'[1:6] <- 0
dat_countryBE$'Last7Days'[is.na(dat_countryBE$Date)] <- 0
dat_countryBE$'Avg7Days'[is.na(dat_countryBE$Date)] <- 0

#NL
dat_countryNL <- dat_country[dat_country$Country=="Netherlands",]
dat_countryNL$'Last7Days' <- NA
dat_countryNL$'Avg7Days' <- NA
for (j in 7:ndays_NL) {
  dat_countryNL$'Last7Days'[j] <-  sum(dat_countryNL$DailyCases[(j-6):j])
  dat_countryNL$'Avg7Days'[j] <-  mean(dat_countryNL$DailyCases[(j-6):j])
}
dat_countryNL$'Last7Days'[1:6] <- 0
dat_countryNL$'Avg7Days'[1:6] <- 0
dat_countryNL$'Last7Days'[is.na(dat_countryNL$Date)] <- 0
dat_countryNL$'Avg7Days'[is.na(dat_countryNL$Date)] <- 0

#DE
dat_countryDE <- dat_country[dat_country$Country=="Germany",]
dat_countryDE$'Last7Days' <- NA
dat_countryDE$'Avg7Days' <- NA
for (j in 7:ndays_DE) {
  dat_countryDE$'Last7Days'[j] <-  sum(dat_countryDE$DailyCases[(j-6):j])
  dat_countryDE$'Avg7Days'[j] <-  mean(dat_countryDE$DailyCases[(j-6):j])
}
dat_countryDE$'Last7Days'[1:6] <- 0
dat_countryDE$'Avg7Days'[1:6] <- 0
dat_countryDE$'Last7Days'[is.na(dat_countryDE$Date)] <- 0
dat_countryDE$'Avg7Days'[is.na(dat_countryDE$Date)] <- 0


#merging
dat_country <- rbind(dat_countryBE, dat_countryDE, dat_countryNL)
#sort the data on date and country name
dat_country <- dat_country[order(dat_country$Date, dat_country$Country),]
rownames(dat_country) <- seq(length=nrow(dat_country))
rm(dat_countryBE, dat_countryDE, dat_countryNL, ndays_BE, ndays_DE, ndays_NL)
dat_country$DailyCasesPer100000 <- dat_country$DailyCases/dat_country$Population * 100000
dat_country$Last7DaysPer100000 <- dat_country$Last7Days/dat_country$Population * 100000
dat_country$Avg7DaysPer100000 <- dat_country$Avg7Days/dat_country$Population * 100000





save(dat, file="./Data/prepared_data/Merged_province_level_data.RData")
save(dat_country, file="./Data/prepared_data/Merged_country_level_data.RData")

###########################################################################################
########################################   PLOTS   ########################################
###########################################################################################


########################
# PROVINCE-LEVEL PLOTS
########################

list_countries <- levels(dat$Country)

### NUMBER OF DAILY CASES
for (i in seq_along(list_countries)) {
  windows()
  dat_plot <- dat[dat$Country==list_countries[i],]
  print(ggplot(dat_plot, aes(fill=Province, y=DailyCases,x=Date)) +geom_bar(position="stack", stat="identity") 
        + labs(subtitle=paste("Daily Reported Cases",list_countries[i])) )
  }


### NUMBER OF DAILY CASES IN LAST SEVEN DAYS
for (i in seq_along(list_countries)) {
  windows()
  dat_plot <- dat[dat$Country==list_countries[i],]
  print(ggplot(dat_plot, aes(fill=Province, y=Last7Days,x=Date)) +geom_line(aes(colour = Province))
        + labs(subtitle=paste("New Cases in Last 7 Days",list_countries[i])) )
}
   
  
### AVERAGE NUMBER OF DAILY CASES (CALCULATED OVER LAST 7 DAYS)
for (i in seq_along(list_countries)) {
  windows()
  dat_plot <- dat[dat$Country==list_countries[i],]
  print(ggplot(dat_plot, aes(fill=Province, y=Avg7Days,x=Date)) +geom_bar(position="stack", stat="identity") 
        + labs(subtitle=paste("Average Number of Daily Cases of Last 7 Days",list_countries[i])) )
}
for (i in seq_along(list_countries)) {
  windows()
  dat_plot <- dat[dat$Country==list_countries[i],]
  print(ggplot(dat_plot, aes(fill=Province, y=Avg7Days,x=Date)) +geom_line(aes(colour = Province))
        + labs(subtitle=paste("Average Number of Daily Cases of Last 7 Days",list_countries[i])) )
}


### COVID INCIDENCE PER 100 000 INHABITANTS: NEW CASES OVER LAST 7 DAYS
for (i in seq_along(list_countries)) {
  windows()
  dat_plot <- dat[dat$Country==list_countries[i],]
  print(ggplot(dat_plot, aes(fill=Province, y=Last7DaysPer100000,x=Date)) +geom_bar(position="stack", stat="identity") 
        + labs(subtitle=paste("New Cases Over Last 7 Days per 100.000 Inhabitants",list_countries[i])) )
}
for (i in seq_along(list_countries)) {
  windows()
  dat_plot <- dat[dat$Country==list_countries[i],]
  print(ggplot(dat_plot, aes(fill=Province, y=Last7DaysPer100000,x=Date)) +geom_line(aes(colour = Province))
        + labs(subtitle=paste("New Cases over Last 7 Days per 100.000 Inhabitants",list_countries[i])) )
}


### COVID INCIDENCE PER 100 000 INHABITANTS: AVERAGE NEW CASES OVER LAST 7 DAYS
for (i in seq_along(list_countries)) {
  windows()
  dat_plot <- dat[dat$Country==list_countries[i],]
  print(ggplot(dat_plot, aes(fill=Province, y=Avg7DaysPer100000,x=Date)) +geom_bar(position="stack", stat="identity") 
        + labs(subtitle=paste("Average Daily Cases Over Last 7 Days per 100.000 Inhabitants",list_countries[i])) )
}
for (i in seq_along(list_countries)) {
  windows()
  dat_plot <- dat[dat$Country==list_countries[i],]
  print(ggplot(dat_plot, aes(fill=Province, y=Avg7DaysPer100000,x=Date)) +geom_line(aes(colour = Province))
        + labs(subtitle=paste("Average Daily Cases Over Last 7 Days per 100.000 Inhabitants",list_countries[i])) )
}





##########################
### COUNTRY-LEVEL PLOTS
##########################

### DAILY NUMBER OF NEW CASES 
windows()
print(ggplot(dat_country, aes(x=Date, y=DailyCases,group=Country, color=Country)) + geom_line()
      + labs("Number of Daily Cases of Last 7 Days") )


#### AVERAGE NUMBER OF NEW CASES (CALCULATED OVER LAST 7 DAYS)
windows()
print(ggplot(dat_country, aes(fill=Country, y=Avg7Days,x=Date)) +geom_line(aes(colour = Country)) 
        + labs("Average Number of Daily Cases of Last 7 Days") )


#### COVID INCIDENCE: DAILY CASES PER 100 000 INHABITANTS
windows()
print(ggplot(dat_country, aes(fill=Country, y=DailyCasesPer100000,x=Date)) +geom_line(aes(colour = Country)) 
      + labs("Daily Cases Per 100.000 Inhabitants") )


#### COVID INCIDENCE: NEW CASES IN LAST 7 DAYS PER 100 000 INHABITANTS
windows()
print(ggplot(dat_country, aes(fill=Country, y=Last7DaysPer100000,x=Date)) +geom_line(aes(colour = Country)) 
      + labs("Number of New Cases In Last 7 Days Per 100.000 Inhabitants") )


#### COVID INCIDENCE: AVERAGE NUMBER OF DAILY CASES (OVER LAST 7 DAYS) PER 100 000 INHABITANTS
windows()
print(ggplot(dat_country, aes(fill=Country, y=Avg7DaysPer100000,x=Date)) +geom_line(aes(colour = Country)) 
      + labs("Average Daily Cases Over Last 7 Days per 100.000 Inhabitants") )
