##############################################
###                                        ###
###        Preparing Population Data       ###
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
  project_path <- "C:/PhD KU Leuven/Projects_Github/RetailCOVID19"
}
setwd(project_path)



#load libraries - you have to install the packages before running the code!
list.of.packages <- c("")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)


####################
# Loading data
####################

#raw dataset
raw_popNL <- read.csv("./Data/population_data/Dutch_population_by_province_2018-2020.csv", skip=4, sep=";")
raw_popBE <- read.csv("./Data/population_data/Belgian_population_by_region_2010-2020.csv", sep=";")
raw_popDE <- read.csv("./Data/population_data/German_population_by_bundeslage_2010-2019.csv", sep=";", skip=5)

#dataframe to manipulate
popNL <- data.frame(raw_popNL)
popBE <- data.frame(raw_popBE)
popDE <- data.frame(raw_popDE)





#######################
# Inspecting data
#######################

#only keep 2020 figures NL
popNL <- popNL[-37,]
popNL <- popNL[popNL$Perioden=="2020",]
popNL <- popNL[,c(1,3)]
#renaming so it matches COVID cases data
names(popNL)[1] <- "Province"
names(popNL)[2] <- "Population"
popNL$Province <- gsub("[(PV) ]","",popNL$Province)
popNL$Province[popNL$Province=="Fryslân"] <- "Friesland"
str(popNL)
popNL$Province <- factor(popNL$Province)
summary(popNL)


#only keeping 2020 figures BE
popBE <- popBE[-4,c(2,13)]
#renaming columns so they match COVID data
names(popBE)[1] <- "Province"
names(popBE)[2] <- "Population"
#renaming provinces so they match COVID data
popBE$Province <- c("Flanders","Brussels","Wallonia")
str(popBE)
popBE$Province <- factor(popBE$Province)
summary(popBE)


popDE <- popDE[-c(17:20),]
popDE <- popDE[,c(1,11)]
names(popDE)[1] <- "Province"
names(popDE)[2] <- "Population"
str(popDE)
popDE$Province <- factor(popDE$Province)
summary(popDE)





#####################################
# Preparing the population dataset
#####################################

population <- rbind(popNL, popBE, popDE)
population$Province <- factor(population$Province)


save(population, file="./Data/prepared_data/2020_population_figures.RData")
