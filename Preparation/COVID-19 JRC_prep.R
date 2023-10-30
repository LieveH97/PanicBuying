##############################################################
#####
##### COVID-19 Health Figures data prep
#####
##############################################################

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
list.of.packages <- c("car")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)




####################
# Loading data
####################

#raw dataset
raw <- read.delim("./Data/covid_data/covid_cases.txt", sep=",", stringsAsFactors = T) 
raw$Date <- as.Date(raw$Date,format="%Y-%m-%d")

#dataframe to make changes in
complete_health <- raw
complete_health$Date <- as.Date(complete_health$Date,format="%Y-%m-%d")






#######################
# Inspecting data
#######################

str(raw)
summary(raw)
#PROBLEMS:
   #Region == "NOT SPECIFIED" for 3330 observations
       table(raw$CountryName, raw$Region=="NOT SPECIFIED")   #in various countries, incl. FR, BE, DE, UK and NE
  #SOLUTION: drop observations since analysis is on regional level

   #lat and lon both have 4865 NAs     --> not very relevant

   #CumulativePositive has 379 NAs
       table(raw$CountryName,is.na(raw$CumulativePositive))  #not the case for 5 countries of interest
       table(raw$Date, is.na(raw$CumulativePositive))        #spread across time, not just in early days where it could've been 0
       
   #CumulativePositive has a minimum of -255 < 0
       table(raw$CountryName, raw$CumulativePositive<0)      #in 3 cases, 2 in UK 
       #PROBLEM! to calculate DailyPositive recursively per region
       #SOLUTION: replace with 0 or mean?
       
   #CumulativeDeceased has 12630 NAs
       table(raw$CountryName, is.na(raw$CumulativeDeceased)) #incl. DE, FR, NE and UK (not BE)
       table(raw$Date, is.na(raw$CumulativeDeceased))        #spread across time, not just in early days where it could've been 0
       #PROBLEM! to calculate DailyDeceased recursively per region
       #SOLUTION: replace with 0 or mean?
    
   #CumulativeRecovered has 23071 NAs
       table(raw$CountryName, is.na(raw$CumulativeRecovered)) #incl. BE, DE, FR, NE and UK
       table(raw$Date, is.na(raw$CumulativeRecovered))        #spread across time, not just in early days where it could've been 0
       #PROBLEM! to calculate DailyRecovered recursively per region
       #SOLUTION: replace with 0 or mean?
       
   #Currently Positive has a minimum of -243700 < 0
       table(raw$CountryName, raw$CurrentlyPositive<0)       #incl. DE, FR, NE and UK (not BE)
       #PROBLEM: to initialize DailyPositive variable
       #SOLUTION: replace with 0?
       
   #Hospitalized has 36827 NAs
       table(raw$CountryName, is.na(raw$Hospitalized))       #incl. DE, FR, NE and UK (not BE)
       # SOLUTION: replace with 0 or mean?

   #IntensiveCare has 37513 NAs
       table(raw$CountryName, is.na(raw$IntensiveCare))      #incl. DE, FR, NE and UK (not BE)
       # SOLUTION: replace with 0 or mean?
       
       



##############################
# Imputing missing data
##############################

### SOLVE CUMULATIVEPOSITIVE < 0 ###

  #replace it by zero
complete_health$CumulativePositive <- replace(complete_health$CumulativePositive,complete_health$CumulativePositive<0,0)

       
       
       
### SOLVE CURRENTLYPOSITIVE < 0 ###
  
  #replace it by zero
complete_health$CurrentlyPositive <- replace(complete_health$CurrentlyPositive,complete_health$CurrentlyPositive<0,0)
       



### SOLVE MISSING VALUES ###   

#SOLUTION 1: replace NAs by zero
   #complete_health$CumulativePositive <- replace(complete_health$CumulativePositive, is.na(complete_health$CumulativePositive),0)
   #complete_health$CumulativeDeceased <- replace(complete_health$CumulativeDeceased, is.na(complete_health$CumulativeDeceased),0)
   #complete_health$CumulativeRecovered <- replace(complete_health$CumulativeRecovered, is.na(complete_health$CumulativeRecovered),0)
   #complete_health$Hospitalized <- replace(complete_health$Hospitalized, is.na(complete_health$Hospitalized),0)
   #complete_health$IntensiveCare <- replace(complete_health$IntensiveCare, is.na(complete_health$IntensiveCare),0)

#SOLUTION 2a: replace Nas by mean
   #complete_health$CumulativePositive <- replace(complete_health$CumulativePositive,is.na(complete_health$CumulativePositive),mean(complete_health$CumulativePositive, na.rm=T))
   #complete_health$CumulativeDeceased <- replace(complete_health$CumulativeDeceased, is.na(complete_health$CumulativeDeceased),mean(complete_health$CumulativeDeceased, na.rm = T))
   #complete_health$CumulativeRecovered <- replace(complete_health$CumulativeRecovered, is.na(complete_health$CumulativeRecovered),mean(complete_health$CumulativeRecovered,na.rm =T))
   #complete_health$Hospitalized <- replace(complete_health$Hospitalized, is.na(complete_health$Hospitalized),mean(complete_health$Hospitalized, na.rm=T))
   #complete_health$IntensiveCare <- replace(complete_health$IntensiveCare, is.na(complete_health$IntensiveCare),mean(complete_health$IntensiveCare, na.rm=T))

#SOLUTION 2b: replace NAs for CumulativeDeceased and CumulativeRecovered by mean OF THAT REGION
#way too slow
   #for (i in 1:length(complete_health$Date)) {
   #  complete_health$CumulativePositive[i] <- ifelse(is.na(complete_health$CumulativePositive[i]),mean(complete_health$CumulativePositive[complete_health$Region==complete_health$Region[i]],na.rm=T),complete_health$CumulativePositive[i])
   #  complete_health$CumulativeDeceased[i] <- ifelse(is.na(complete_health$CumulativeDeceased[i]),mean(complete_health$CumulativeDeceased[complete_health$Region==complete_health$Region[i]],na.rm=T), complete_health$CumulativeDeceased[i])
   #  complete_health$CumulativeRecovered[i] <- ifelse(is.na(complete_health$CumulativeRecovered[i]),mean(complete_health$CumulativeRecovered[complete_health$Region==complete_health$Region[i]],na.rm=T), complete_health$CumulativeRecovered[i])
   #  complete_health$Hospitalized[i] <- ifelse(is.na(complete_health$Hospitalized[i]),mean(complete_health$Hospitalized[complete_health$Region==complete_health$Region[i]],na.rm=T),complete_health$Hospitalized[i])
   #  complete_health$IntensiveCare[i] <- ifelse(is.na(complete_health$IntensiveCare[i]),mean(complete_health$IntensiveCare[complete_health$Region==complete_health$Region[i]],na.rm=T), complete_health$IntensiveCare[i])
   #}

#SOLUTION 3: replace NAs of Cumulative variables by previous observation of that region 
#if value of first observation is NA --> make it 0
#order dataset according to region - date
   complete_health <- complete_health[order(complete_health$Region),]
   rownames(complete_health) <- seq(length=nrow(complete_health))
   #replacing NAs CumulativePositive with previous observation of Region (or 0 if it's the first)
   for (i in seq_along(complete_health$Date)) {
     if (is.na(complete_health$CumulativePositive[i])) {
       complete_health$CumulativePositive[i] <- ifelse(complete_health[i-1,]$Region == complete_health[i,]$Region,complete_health$CumulativePositive[i-1],0)
     }
   }
   #replacing NAs CumulativeDeceased with previous observation of Region (or 0 if it's the first)
   complete_health$CumulativeDeceased[1] <- 0    #otherwise error since first observation is NA and i-1 doesn't work then
   for (i in seq_along(complete_health$Date)) {
     if (is.na(complete_health$CumulativeDeceased[i])) {
       complete_health$CumulativeDeceased[i] <- ifelse(complete_health[i-1,]$Region == complete_health[i,]$Region,complete_health$CumulativeDeceased[i-1],0)
     }
   }
   #replacing NAs CumulativeRecovered with previous observation of Region (or 0 if it's the first)
   complete_health$CumulativeRecovered[1] <- 0    #otherwise error since first observation is NA and i-1 doesn't work then
   for (i in seq_along(complete_health$Date)) {
     if (is.na(complete_health$CumulativeRecovered[i])) {
       complete_health$CumulativeRecovered[i] <- ifelse(complete_health[i-1,]$Region == complete_health[i,]$Region,complete_health$CumulativeRecovered[i-1],0)
     }
   }
   #replacing NAs Hospitalized with previous observation of Hospitalized of that Region (or 0 if it's the first)
   complete_health$Hospitalized[1] <- 0    #otherwise error since first observation is NA and i-1 doesn't work then
   for (i in seq_along(complete_health$Date)) {
     if (is.na(complete_health$Hospitalized[i])) {
       complete_health$Hospitalized[i] <- ifelse(complete_health[i-1,]$Region == complete_health[i,]$Region,complete_health$Hospitalized[i-1],0)
     }
   }
   #replacing NAs ICU with previous observation of ICU of that Region (or 0 if it's the first)
   complete_health$IntensiveCare[1] <- 0     #otherwise error since first observation is NA and i-1 doesn't work then
   for (i in seq_along(complete_health$Date)) {
     if (is.na(complete_health$IntensiveCare[i])) {
        complete_health$IntensiveCare[i] <- ifelse(complete_health[i-1,]$Region == complete_health[i,]$Region,complete_health$IntensiveCare[i-1],0)
     }
   }
   #back to original ordering
   complete_health <- complete_health[order(complete_health$Date, complete_health$CountryName),]
   rownames(complete_health) <- seq(length=nrow(complete_health))

   
   
   
### SOLVE UNSPECIFIED REGIONS ###
   #exclude observations for which Region = NOT SPECIFIED (since we'll analyze on regional level)
complete_health <- complete_health[complete_health$Region!="NOT SPECIFIED",]



#check for all countries
str(complete_health)       #86314 observations (down from 89644)
summary(complete_health)

#check for countries of interest
list_interest_countries <- c("Belgium", "Germany", "France", "Netherlands", "United Kingdom")
str(complete_health[complete_health$CountryName %in% list_interest_countries,])   #14275 observations (down from 15059)





########################################################################################
### New Variables: Daily New Cases, Daily Deaths, Daily Recovered, Daily Hospitalized
########################################################################################

complete_health$DailyPositive <- NA
complete_health$DailyDeath <- NA
complete_health$DailyRecovered <- NA

complete_health <- complete_health[,c(1:6,16,7,17,8,18,9:15)]

### create list with countries and their regions
all_regions <- data.frame(region_id=1:length(unique(complete_health$Region)),region_name=unique(complete_health$Region), country=NA)
for (i in seq_along(all_regions$region_id)) {
  all_regions$country[i] <- as.character(unique(complete_health$CountryName[complete_health$Region==all_regions$region_name[i]]))
}



#### DAILY POSITIVE ####

### create DailyPositive variable as running difference of CumulativePositive within each region
#first value of DailyPositive can't be calculated that way (because i-1 for i=1 doesn't work) --> so first DailyPositive = CurrentlyPositive
for (i in seq_along(all_regions$region_id)) {
  complete_health$DailyPositive[complete_health$Region==all_regions$region_name[i]] <- c(complete_health$CurrentlyPositive[complete_health$Region==all_regions$region_name[i]][1], diff(complete_health$CumulativePositive[complete_health$Region==all_regions$region_name[i]]))
}

      #inspecting DailyPositive
      sort(complete_health$DailyPositive)      #DailyPositive <0 for 939 observations 
            #--> probably because countries adjust stats afterwards (e.g. exclude double counts etc.)
      table(complete_health$CountryName,complete_health$DailyPositive<0)   #the case for DE, NE, FR and UK (not BE)
      #not sure how to solve this: further along, I just exclude these negative values


      
#### DAILY DEATH ####      

### create DailyDeath variable as running difference of CumulativeDeceased within each region
#assuming a region's first observation is of DailyDeath = 0 (reasonable because first a COVID case is reported before first COVID death)
for (i in seq_along(all_regions$region_id)) {
  complete_health$DailyDeath[complete_health$Region==all_regions$region_name[i]] <- c(0, diff(complete_health$CumulativeDeceased[complete_health$Region==all_regions$region_name[i]]))
}

      #inspecting DailyDeath
      sort(complete_health$DailyDeath)      #DailyDeath <0 for 296 observations 
      #--> probably because countries adjust stats afterwards (e.g. exclude double counts etc.)
      table(complete_health$CountryName,complete_health$DailyDeath<0)   #the case for BE, DE, NE, FR and UK
      #not sure how to solve this: further along, I just exclude these negative values
      
      

#### DAILY RECOVERED ####      
            
### create DailyRecovered variable as running difference of CumulativeRecovered within each region
#assuming a region's first observation is of DailyRecovered = 0 (reasonable because first COVID Case is reported before COVID recovery)
for (i in seq_along(all_regions$region_id)) {
  complete_health$DailyRecovered[complete_health$Region==all_regions$region_name[i]] <- c(0, diff(complete_health$CumulativeRecovered[complete_health$Region==all_regions$region_name[i]]))
}

      #inspecting DailyRecovered
      sort(complete_health$DailyRecovered)      #DailyRecovered <0 for 244 observations 
      #--> probably because countries adjust stats afterwards (e.g. exclude double counts etc.)
      table(complete_health$CountryName,complete_health$DailyRecovered<0)   #case for FR 
      #not sure how to solve this: further along, I just exclude these negative values
      

#### DAILY HOSPITALIZED ####          
#did not create a DailyHospitalized variable
#according to JRC "hospitalized" is cumulative
#but when plotted on https://covid-statistics.jrc.ec.europa.eu/Home/Dashboard and here:
plot(complete_health$Date[complete_health$CountryName=="Belgium"], complete_health$Hospitalized[complete_health$CountryName=="Belgium"])
   #Belgium: curve not cumulative --> daily hospitalized
plot(complete_health$Date[complete_health$CountryName=="France"], complete_health$Hospitalized[complete_health$CountryName=="France"])
   #France: curve not cumulative --> daily hospitalized
plot(complete_health$Date[complete_health$CountryName=="Germany"], complete_health$Hospitalized[complete_health$CountryName=="Germany"])
   #Germany: Hospitalized = 0 every day
plot(complete_health$Date[complete_health$CountryName=="Netherlands"], complete_health$Hospitalized[complete_health$CountryName=="Netherlands"])
   #Netherlands: cumulative, but only observations from mid july onwards
plot(complete_health$Date[complete_health$CountryName=="United Kingdom"], complete_health$Hospitalized[complete_health$CountryName=="United Kingdom"])
   #UK: Hospitalized = 0 every day

summary(complete_health)
head(complete_health)
some(complete_health, 20)





###########################################################
### Construct data subset: BE + DE + NL + FR + UK
###########################################################

#list of regions in countries subset
interest_regions <- NULL
list_interest_countries <- c("Belgium", "Germany", "France", "Netherlands", "United Kingdom")
for (i in seq_along(all_regions$country)) {
  if (all_regions$country[i] %in% list_interest_countries) {
    interest_regions <- rbind(interest_regions, c(all_regions$region_id[i], as.character(all_regions$region_name[i]), as.character(all_regions$country[i])))
  }
}
interest_regions <- data.frame(interest_regions)
colnames(interest_regions) <- c("Region_id", "Region", "CountryName")


#COVID daily health data for regions/countries in subset
sub_health <- complete_health[complete_health$CountryName %in% list_interest_countries,]
sub_health$Date <- as.Date(sub_health$Date, format="%Y-%m-%d")

str(sub_health)
  #PROBLEM: other country/region names are still factors
sub_health$CountryName <- factor(sub_health$CountryName)
sub_health$Region <- factor(sub_health$Region)
sub_health$iso3 <- factor(sub_health$iso3)
sub_health$NUTS <- factor(sub_health$NUTS)
rownames(sub_health) <- seq(length=nrow(sub_health))

summary(sub_health)





### FIXING DAILYPOSITIVE < 0 ###

boxplot(sub_health$DailyPositive)
#at country level: according to Worldometer.info, max daily cases reported is +-18000 (UK)
#what boundary for regional level?
#excluded all outliers (with boxplot$out) is way too strict
#therefore, setting al negative values and extreme outliers equal to 0 
for (i in seq_along(sub_health$Date)) {
  if (sub_health$DailyPositive[i]>0 & sub_health$DailyPositive[i]<20000) {
  sub_health$DailyPosFix[i] <- sub_health$DailyPositive[i]} 
  else { sub_health$DailyPosFix[i] <- 0}
}
boxplot(sub_health$DailyPosFix)     

#test a UK region
plot(sub_health$Date[sub_health$Region=="London"], sub_health$DailyPosFix[sub_health$Region=="London"])

#PROBLEMS:
#UK: many large outliers (mess up the scale of the graph)
    #due to sudden jumps in CumulativePositive curve in raw dataset
    plot(raw$Date[raw$Region=="South East"], raw$CumulativePositive[raw$Region=="South East"])
    #solution: set stricter bound in if loop?
#FR: for all regions, DailyPosFix = 0 from March 26 onwards 
    #due to CumulativePositive = 0 from March 26 onwards in raw dataset
    raw[raw$CountryName=="France" & raw$CumulativePositive==0, ]


    
    
    
### FIXING DAILYDEATH < 0 ###

#PROBLEM: DailyDeath has many negative values and extreme outliers
boxplot(sub_health$DailyDeath)
sort(boxplot(sub_health$DailyDeath)$out, decreasing=T)
#according to worldometer.info, max number of daily deaths reported on COUNTRY level +-2500
#what boundary for regional level?
#solution: setting all negative values and extreme outliers equal to 0
for (i in seq_along(sub_health$Date)) {
  if (sub_health$DailyDeath[i]>0 & sub_health$DailyDeath[i]<2500) {
    sub_health$DailyDeathFix[i] <- sub_health$DailyDeath[i]} 
  else { sub_health$DailyDeathFix[i] <- 0}
}
boxplot(sub_health$DailyDeathFix)     

#test a FR region
plot(sub_health$Date[sub_health$Region=="Bretagne"], sub_health$DailyDeathFix[sub_health$Region=="Bretagne"])

#PROBLEMS:
#UK: all UK regions, except England, Wales, Scotland
    #most have DailyDeathFix=0 every day
    #due to zeros for CumulativeDeceased in raw dataset
    some(raw[raw$CountryName=="United Kingdom",],40)
    #some have a few (high) observations: North East And Yorkshire, Northern Ireland, Greater Glasgow And Clyde
#NL: all NL regions
    #most have in raw dataset CumulativeDeceased=0 until July 18, only then daily increase
    plot(raw$Date[raw$Region=="Limburg"], raw$CumulativeDeceased[raw$Region=="Limburg"])
    #this makes that DailyDeathFix=0, then spikes (600+ in some regions) on July 18 and then normal behavior 
    #exception: Friesland -> zero until July 18 (spike) then zero again
    #exception: Groningen -> zero until July 18 (spike) then zero again, then another spike (August 25), then zero again
#DE: region "Repatriierte" gives strange graph --> only has four observations with mostly missing data
    raw[raw$Region=="Repatriierte",]
    #exclude?




### FIXING DAILYRECOVERED < 0 ###

#PROBLEM: DailyRecovered has negative values and a few very large outliers
boxplot(sub_health$DailyRecovered)
sort(boxplot(sub_health$DailyRecovered)$out, decreasing=T)
#according to worldometer.info, max number of daily deaths reported on COUNTRY level +-10200 (DE)
#what boundary for regional level?
#solution: setting all negative values and extreme outliers equal to 0
for (i in seq_along(sub_health$Date)) {
   if (sub_health$DailyRecovered[i]>0 & sub_health$DailyRecovered[i]<10200) {
      sub_health$DailyRecovFixed[i] <- sub_health$DailyRecovered[i]} 
   else { sub_health$DailyRecovFixed[i] <- 0}
}

#PROBLEMS:
#UK: all regions have DailyRecovFixed = 0 every day
     #due to zeros and missing data for CumulativeRecovered in raw dataset
     some(raw[raw$CountryName=="United Kingdom",],40)
#NL: all regions have DailyRecovFixed = 0 every day
     #due to zeros and missing data for CumulativeRecovered in raw dataset
     some(raw[raw$CountryName=="Netherlands",],40)
#FR: four regions (Auvergne-Rhône-Alpes,Hauts-de-France,Occitanie,Provence-Alpes-Côte d'Azur) have very large outliers
     #due to a drop of CumulativeRecovered to zero in raw dataset in July
     plot(raw$Date[raw$Region=="Occitanie"], raw$CumulativeRecovered[raw$Region=="Occitanie"])
#DE: 15 out of 17 regions in Germany have DailyRecovFixed = 0 every day
     #due to zeros and missing data for CumulativeRecovered in raw dataset
     some(raw[raw$CountryName=="Germany",],40)
#BE: all regions have DailyRecovFixed = 0 every day
     #due to missing data for CumulativeRecovered in raw dataset
     some(raw[raw$CountryName=="Belgium",],40)
     






################################
### Plotting Regions
################################


#Daily Cases
plot(sub_health$Date[sub_health$Region=="Flanders"], sub_health$DailyPosFix[sub_health$Region=="Flanders"])


#Daily Deaths         
plot(sub_health$Date[sub_health$Region=="Scotland"], sub_health$DailyDeathFix[sub_health$Region=="Scotland"])

#Daily Recovered
plot(sub_health$Date[sub_health$Region=="Bretagne"], sub_health$DailyRecovFixed[sub_health$Region=="Bretagne"])
         
#Currently Positive
plot(sub_health$Date[sub_health$Region=="Hessen"], sub_health$CurrentlyPositive[sub_health$Region=="Hessen"])
   #looks like currently positive is also cumulative? Doesn't go down         





################################
### Saving the Dataframe
################################

save(sub_health, file="./R_Files/Preparation/dataJRC.RData")
