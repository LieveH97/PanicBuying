##############################################
###                                        ###
###      Filtering households              ###
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


### set path to data files
if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
  data_path <- ""
}else if (Sys.info()["nodename"] == "LAPTOP-KBL77S8J"){
  data_path <- ""
}else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
  data_path <- "C:/PhD KU Leuven/AiMark"
}


#load libraries - you have to install the packages before running the code!
list.of.packages <- c("plyr")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)

# create country list
country_list <- c("NL","BE","DE","FR","UK")





#########################
# Loading household data
#########################
# set country
country <- country_list[country_list=="NL"]

#raw dataset
raw_HH <- read.csv(paste(data_path,"/",country,"/panelist.csv",sep=""))
#dataset to manipulate
HH <- raw_HH



###########################
# Inspecting household data
###########################
str(HH)
HH$Panelist <- factor(HH$Panelist)
HH$Quarter <- factor(HH$Quarter)
HH$Postal_code <- factor(HH$Postal_code)
HH$Age <- factor(HH$Age,levels = c(1:11),labels = c("30-34year", "25-29year", "20-24year",
                 "12-19year","75+year","55-64year","65-74year", "45-49year","50-54year",
                 "35-39year","40-45year")) 
HH$Household_size <- factor(HH$Household_size,levels=c(1:5), labels=c("1 household member",
                            "2 household members","3 household members","4 household members",
                            "5 or more household members"))
HH$Social_class <- factor(HH$Social_class, levels=c(1:6),labels=c("A","Unknown","D","C",
                          "B-minus","B-plus"))
HH$Income_class <- factor(HH$Income_class,levels=c(1,6,5,4,3,20,19,22,21,16,15,18,17,12,11,
                          14,13,9,7),labels=c("Below 700 euro","700-900 euro","900-1100 euro",
                          "1100-1300 euro","1300-1500 euro","1500-1700 euro","1700-1900 euro",
                          "1900-2100 euro","2100-2300 euro","2300-2500 euro","2500-2700 euro",
                          "2700-2900 euro","2900-3100 euro","3100-3300 euro","3300-3500 euro",
                          "3500-3700 euro","3700-3900 euro","3900-4100 euro","4100 euro or more"))
HH$Region <- factor(HH$Region)
HH$Province <- factor(HH$Province)



#######################################
# drop HHs that left panel before 2018
#######################################

# create year variable = first 4 numbers of Quarter variable
HH$Year <- as.numeric(substr(HH$Quarter,1,4))
HH$Year <- factor(HH$Year)

# list of panelist IDs + to store outcome of test
panelist <- data.frame(ID=unique(HH$Panelist),filterOK=NA)
years_list <- c(2018,2019,2020)
#length(panelist$ID)
# check whether last value for 'YEAR' is 2018, 2019 OR 2020
for (i in seq_along(panelist$ID)) {
  panelist$filterOK[i] <- ifelse(tail(HH$Year[HH$Panelist==panelist$ID[i]],1)%in%years_list,1,0) }
table(panelist$filterOK)

# exclude panelists that left before 2018 from HH dataset
panelists_keep_ID <- as.vector(panelist$ID[panelist$filterOK==1])
HH <- HH[HH$Panelist %in% panelists_keep_ID,]

# factor levels changed
HH$Panelist <- factor(HH$Panelist)
HH$Postal_code <- factor(HH$Postal_code)
HH$Year <- factor(HH$Year)

rm(list=c('panelists_keep_ID','panelist'))


######################
# load purchase data
######################
#raw datasets
raw_purch2018 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2018.csv",sep=""))
raw_purch2019 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2019.csv",sep=""))
raw_purch2020 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2020.csv",sep=""))

#dataset to manipulate
purch2018 <- raw_purch2018
purch2019 <- raw_purch2019
purch2020 <- raw_purch2020

#combine all three purchase files
purch <- rbind(purch2018, purch2019, purch2020)
rm(list=c('purch2018','purch2019','purch2020'))



###########################
# Inspecting purchase data
###########################
str(purch)
purch$Panelist <- factor(purch$Panelist)
purch$Date_of_purchase <- as.Date(purch$Date_of_purchase)
purch$Banner_name <- factor(purch$Banner_name)
purch$Barcode <- factor(purch$Barcode)
purch$Quarter <- factor(purch$Quarter)

purch$Promo <- ifelse(purch$Promo=="yes",1,0)
purch$Promo <- as.logical(purch$Promo)

purch$Purchase_method <- factor(purch$Purchase_method)



################################
# merging purchase and HH data
################################

HH_purch <- merge(purch, HH, by=c("Panelist","Quarter"))




###############################################
# filter the household panels based on purchase
###############################################
# !!!
# household is out of the analysis if at any point, they do not buy anything 
# for a whole quarter/year

quarter_20182020 <- c("201801","201802", "201803","201804","201901","201902","201903",
                      "201904","202001","202002","202003","202004")
quarter_20192020 <- c("201901","201902","201903","201904","202001","202002","202003","202004")
year_20182020 <- c("2018", "2019", "2020")
year_20192020 <- c("2019", "2020")


# function to determine whether panelist made purchase in every Quarter or Year
filter_minpurchase_quarter <- function(df, quarterlist) {
  # create dataframe with all panelist ID and a flag 
  panelists <- data.frame(ID=unique(df$Panelist),filterOK=NA)
  # aggregate purchases up to panelist-quarter level
  df <- ddply(df,.(Panelist, Quarter), summarise, Total_unit_sales=sum(Total_unit_sales),
              Total_value_sales=sum(Total_value_sales))
  # for every panelist --> make sure as many unique (because ddply) quarters as in list
  for (i in seq_along(panelists$ID)) {
    panelists$filterOK[i] <- ifelse(sum(df$Quarter[df$Panelist==panelists$ID[i]] %in% quarterlist)==length(quarterlist),1,0) }
  return(panelists)
}

filter_minpurchase_year <- function(df, yearlist) {
  # create dataframe with all panelist ID and a flag 
  panelists <- data.frame(ID=unique(df$Panelist),filterOK=NA)
  # aggregate purchases up to panelist-year level
  df <- ddply(df,.(Panelist, Year), summarise, Total_unit_sales=sum(Total_unit_sales),
              Total_value_sales=sum(Total_value_sales))
  # for every panelist --> make sure as many unique (because ddply) years as in list
  for (i in seq_along(panelists$ID)) {
      panelists$filterOK[i] <- ifelse(sum(df$Year[df$Panelist==panelists$ID[i]] %in% yearlist)==length(yearlist),1,0) }
  return(panelists)
}

# QUARTER - 2018-2020
panelists_Q_20182020 <- filter_minpurchase_quarter(HH_purch, quarter_20182020)
table(panelists_Q_20182020$filterOK)
# QUARTER - 2019-2020
panelists_Q_20192020 <- filter_minpurchase_quarter(HH_purch, quarter_20192020)
table(panelists_Q_20192020$filterOK)
# YEAR - 2018-2020
panelists_Y_20182020 <- filter_minpurchase_year(HH_purch, year_20182020)
table(panelists_Y_20182020$filterOK)
# YEAR - 2019-2020
panelists_Y_20192020 <- filter_minpurchase_year(HH_purch, year_20192020)
table(panelists_Y_20192020$filterOK)




#########################################################
# filter the household panels based on presence in panel
#########################################################
# !!!
# household is out of the analysis if they left the panel at any point 

filter_panelpres_quarter <- function(df, quarterlist) {
  # create dataframe with all panelist ID and a flag 
  panelists <- data.frame(ID=unique(df$Panelist),filterOK=NA)
  # for every panelist --> check that for all quarters, they remain in the panel
  for (i in seq_along(panelists$ID)) {
    panelists$filterOK[i] <- ifelse(sum(df$Quarter[df$Panelist==panelists$ID[i]] %in% quarterlist)==length(quarterlist),1,0) }
  return(panelists)
}

filter_panelpres_year <- function(df, yearlist) {
  # create dataframe with all panelist ID and a flag 
  panelists <- data.frame(ID=unique(df$Panelist),filterOK=NA)
  # for every panelist --> check that for all years, they remain in the panel
  for (i in seq_along(panelists$ID)) {
    panelists$filterOK[i] <- ifelse(sum(unique(df$Year[df$Panelist==panelists$ID[i]]) %in% yearlist)==length(yearlist),1,0) }
  return(panelists)
}

# QUARTER - 2018-2020
panelists_Q_20182020_HH <- filter_panelpres_quarter(HH, quarter_20182020)
table(panelists_Q_20182020_HH$filterOK)
# QUARTER - 2019-2020
panelists_Q_20192020_HH <- filter_panelpres_quarter(HH, quarter_20192020)
table(panelists_Q_20192020_HH$filterOK)
# YEAR - 2018-2020
panelists_Y_20182020_HH <- filter_panelpres_year(HH, year_20182020)
table(panelists_Y_20182020_HH$filterOK)
# YEAR - 2019-2020
panelists_Y_20192020_HH <- filter_panelpres_year(HH, year_20192020)
table(panelists_Y_20192020_HH$filterOK)



#######################
# compare both filters
#######################

# ideally, only very few households who are excluded in purchase-based filter are included in panel-based filter
# these are the people who stopped purchasing (instead of left the panel)
sum(panelists_Q_20182020$ID[panelists_Q_20182020$filterOK==1] %in% panelists_Q_20182020_HH$ID[panelists_Q_20182020_HH$filterOK==1])
sum(panelists_Q_20182020$ID[panelists_Q_20182020$filterOK==0] %in% panelists_Q_20182020_HH$ID[panelists_Q_20182020_HH$filterOK==1])

sum(panelists_Q_20192020$ID[panelists_Q_20192020$filterOK==1] %in% panelists_Q_20192020_HH$ID[panelists_Q_20192020_HH$filterOK==1])
sum(panelists_Q_20192020$ID[panelists_Q_20192020$filterOK==0] %in% panelists_Q_20192020_HH$ID[panelists_Q_20192020_HH$filterOK==1])

sum(panelists_Y_20182020$ID[panelists_Y_20182020$filterOK==1] %in% panelists_Y_20182020_HH$ID[panelists_Y_20182020_HH$filterOK==1])
sum(panelists_Y_20182020$ID[panelists_Y_20182020$filterOK==0] %in% panelists_Y_20182020_HH$ID[panelists_Y_20182020_HH$filterOK==1])

sum(panelists_Y_20192020$ID[panelists_Y_20192020$filterOK==1] %in% panelists_Y_20192020_HH$ID[panelists_Y_20192020_HH$filterOK==1])
sum(panelists_Y_20192020$ID[panelists_Y_20192020$filterOK==0] %in% panelists_Y_20192020_HH$ID[panelists_Y_20192020_HH$filterOK==1])





#####################################################
# investigate characteristics of kept vs. dropped HH
#####################################################
# !!!!
# !!!! adjust depending on which scenario/filter !! (Quarter vs. Year, 2018-2020 vs. 2019-2020)
# !!!!

#create vectors with panel ID of panelists we keep and drop
panelists_keep_ID <- as.vector(panelists_Q_20182020$ID[panelists_Q_20182020$filterOK==1])
panelists_drop_ID <- as.vector(panelists_Q_20182020$ID[panelists_Q_20182020$filterOK==0])

HH_keep <- HH[HH$Panelist %in% panelists_keep_ID,]
HH_drop <- HH[HH$Panelist %in% panelists_drop_ID,]

## compare characteristics of the households we exclude with those we keep

#comparing age of households we exclude or keep
age_keep <- prop.table(table(HH_keep$Age))
age_drop <- prop.table(table(HH_drop$Age))
age_OG <- prop.table(table(HH$Age))
age_compare <- rbind(age_OG, age_keep, age_drop)
age_compare
chisq.test(age_keep,age_drop)

#comparing income of households we exclude or keep
income_keep <- prop.table(table(HH_keep$Income_class))
income_drop <- prop.table(table(HH_drop$Income_class))
income_OG <- prop.table(table(HH$Income_class))
income_compare <- round(rbind(income_OG,income_keep, income_drop),5)
income_compare
chisq.test(income_keep,income_drop)

#comparing household size of households we exclude or keep
size_keep <- prop.table(table(HH_keep$Household_size))
size_drop <- prop.table(table(HH_drop$Household_size))
size_OG <- prop.table(table(HH$Household_size))
size_compare <- rbind(size_OG, size_keep, size_drop)
size_compare
chisq.test(size_keep,size_drop)

#comparing social class of households we exclude or keep
social_keep <- prop.table(table(HH_keep$Social_class))
social_drop <- prop.table(table(HH_drop$Social_class))
social_OG <- prop.table(table(HH$Social_class))
social_compare <- rbind(social_OG, social_keep, social_drop)
social_compare
chisq.test(social_keep, social_drop)

#comparing region of households we exclude or keep
region_keep <- prop.table(table(HH_keep$Region))
region_drop <- prop.table(table(HH_drop$Region))
region_OG <- prop.table(table(HH$Region))
region_compare <- rbind(region_OG, region_keep, region_drop)
region_compare
chisq.test(region_keep, region_drop)

#comparing province of households we exclude or keep
province_keep <- prop.table(table(HH_keep$Province))
province_drop <- prop.table(table(HH_drop$Province))
province_OG <- prop.table(table(HH$Province))
province_compare <- round(rbind(province_OG, province_keep, province_drop),5)
province_compare
chisq.test(province_keep, province_drop)



########################################################
# save filtered HH IDs (to be used in category analyses)
########################################################

save(panelists_keep_ID, file=paste("./R_Files/Preparation/household filters/",country,"filtered_HH_ID.RData",sep=""))


