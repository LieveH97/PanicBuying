##############################################
###                                        ###
###      Filtering households              ###
###                                        ###
##############################################

rm(list=ls())


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

location <- "local"

### set path to data files
if (location == "server") {
  if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
    data_path <- "X:/Retailing_Group_RawData/Hoarding data"
  }else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
    data_path <- "X:/Retailing_Group_RawData/Hoarding data" }
} else if (location == "local") { 
  if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
    data_path <- ""
  }else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
    data_path <- "C:/PhD KU Leuven/AiMark" } 
}

### set path to save intermediate files
if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
  save_path <- "X:/Retailing_Group/PhD Lieve/Project 1"
}else if (Sys.info()["nodename"] == "LAPTOP-KBL77S8J"){
  save_path <- ""
}else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
  save_path <- "X:/Retailing_Group/PhD Lieve/Project 1"
}
#load libraries - you have to install the packages before running the code!
list.of.packages <- c("plyr")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)

# create country list
country_list <- c("NL","BE","GE","FR","UK")





#########################
# Loading household data
#########################
# set country
country <- country_list[country_list=="FR"]

#Household characteristics dataset
HH <- read.csv(paste(data_path,"/",country,"/panelist.csv",sep=""))

if (country=="FR") {
  colnames(HH)[2] <- "Quarter"
  colnames(HH)[3] <- "Age"
  colnames(HH)[4] <- "Household_size"
  colnames(HH)[5] <- "Social_class"
  colnames(HH)[6] <- "Region"
  colnames(HH)[7] <- "Postal_code"
  colnames(HH)[11] <- "Income_class"
} else if (country == "UK") {
  colnames(HH)[7] <- "Income_class"
}



###########################
# Inspecting household data
###########################
str(HH)
HH$Panelist <- factor(HH$Panelist)
HH$Quarter <- factor(HH$Quarter)
if (country == "BE") {
} else {HH$Postal_code <- factor(HH$Postal_code)}
if (country=="NL"){
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
}else if (country=="GE") {
  HH$Age <- factor(HH$Age, labels=c("20-24year","25-29year","30-34year","35-39year","40-44year",
                   "45-49year","50-54year","55-59year","60-64year","65-69year","70-74year",
                   "75+year","75+year","-19year"))
  HH$Household_size <- factor(HH$Household_size)
  HH$Social_class <- factor(HH$Social_class, levels=c(1:6),labels=c("Bottom Layer","Lower Middle Class",
                            "Lower Working Class","Middle Middle Class", "Upper Layer", "Upper Working Class"))
  HH$Income_class <- factor(HH$Income_class,levels=c(1:17), labels=c("Below 500 euro","500-749 euro",
                            "750-999 euro", "1000-1249 euro","1250-1499 euro","1500-1749 euro","1750-1999 euro",
                            "2000-2249 euro","2250-2499 euro","2500-2749 euro","2750-2999 euro","3000-3249 euro",
                            "3250-3499 euro","3500-3749 euro","3750-3999 euro","4000-4999 euro","5000 euro or more"))
} else if (country=="FR") {
  HH$Age <- factor(HH$Age)
  HH$Household_size <- factor(HH$Household_size, labels=c("1 household member","2 household members",
                             "3 household members", "4 or more household members"))
  HH$Social_class <- factor(HH$Social_class, labels=c("Very Low","Low", "Medium","High","Very High"))
  HH$Income_class <- factor(HH$Income_class, labels=c("undefined","7000 euro or more","1100-1199 euro","1200-1399 euro",
                            "1500-1899 euro","1900-2299 euro","1400-1499 euro","2300-2699 euro","2700-2999 euro",
                            "3000-3799 euro", "3800-4499 euro","300-449 euro","4500-5399 euro", "450-599 euro",
                            "5400-6999 euro","600-749 euro","750-899 euro","900-1099 euro","Below 300 euro"))
} else if (country=="BE") {
  HH$Age <- factor(HH$Age, labels=c("-34 years", "35-49year","50-64year", "65+year"))
  HH$Household_size <- factor(HH$Household_size, labels=c("undefined", "2 household members",
                              "3 household members", "4 household members","5 or more household members", 
                              "1 household member"))
  HH$Social_class <- factor(HH$Social_class, labels=c("undefined", "High", "Middle", "Low"))
  HH$Income_class <- factor(HH$Income_class, labels=c("undefined", "1984-2726 euro", "1240-1983 euro",
                            "469-1239 euro", "Below 469 euro", "Unknown", "2727 euro or more"))
} else if (country == "UK") {
  HH$Age <- factor(Age)
  HH$Household_size <- factor(HH$Household_size, labels = c("undefined", "1 household member", 
                              "2 household members", "3 household members", "4 household members",
                              "5 household members", "6 household members", "7 household members",
                              "8 household members", "9 household members", "10 household members",
                              "11 household members", "12 household members", "13 household members",
                              "14 household members", "undefined"))
  HH$Social_class <- factor(HH$Social_class, labels=c("Upper Middle Class", "Middle Class", "Lower Middle Class",
                            "Skilled Working Class", "Working Class", "Lowest Level of Substistence"))
  HH$Income_class <- factor(HH$Income_class, labels=c("undefined", "0-9999 pounds/year","10000-19999 pounds/year",
                            "20000-29999 pounds/year", "30000-39999 pounds/year", "40000-49999 pounds/year",
                            "50000-59999 pounds/year","60000-69999 pounds/year", "70000+ pounds/year", "undefined"))
}
if (country == "UK") {
} else { HH$Region <- factor(HH$Region)
}
if (country == "UK" | country == "FR") {
} else {HH$Province <- factor(HH$Province)
}



#######################################
# drop HHs that left panel before 2018
#######################################

# create year variable = first 4 numbers of Quarter variable
HH$Year <- as.numeric(substr(HH$Quarter,1,4))
HH$Year <- factor(HH$Year)

# drop HH info from before 2018
years_list <- c(2018,2019,2020)
total_panelists <- length(unique(HH$Panelist))
HH <- HH[HH$Year %in% years_list,]
remain_panelists <- length(unique(HH$Panelist))

# factor levels changed
HH$Panelist <- factor(HH$Panelist)
if (country == "BE") {
  } else {HH$Postal_code <- factor(HH$Postal_code)}
HH$Year <- factor(HH$Year)

rm(list=c('total_panelists','remain_panelists','years_list'))




######################
# load purchase data
######################
#raw datasets
if (country=="NL"){
  purch2018 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2018.csv",sep=""))
  purch2019 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2019.csv",sep=""))
  purch2020 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2020.csv",sep=""))
} else {
  purch2018 <- read.csv(paste(data_path,"/",country,"/purchase_2018.csv",sep=""))
  purch2019 <- read.csv(paste(data_path,"/",country,"/purchase_2019.csv",sep=""))
  purch2020 <- read.csv(paste(data_path,"/",country,"/purchase_2020.csv",sep=""))
  }

#combine all three purchase files
if (country == "UK") {
} else { purch <- rbind(purch2018, purch2019, purch2020)
         rm(list=c('purch2018','purch2019','purch2020')) } 



###########################
# Inspecting purchase data
###########################
if (country == "UK") {
  purch2018 <- purch2018[,c(1,5,8)]
  purch2018$Panelist <- factor(purch2018$Panelist)
  purch2018$Quarter <- factor(purch2018$Quarter)

  purch2019 <- purch2019[,c(1,5,8)]
  purch2019$Panelist <- factor(purch2019$Panelist)
  purch2019$Quarter <- factor(purch2019$Quarter)
  
  purch2020 <- purch2020[,c(1,5,8)]
  purch2020$Panelist <- factor(purch2020$Panelist)
  purch2020$Quarter <- factor(purch2020$Quarter)
} else { purch$Panelist <- factor(purch$Panelist)
         purch$Date_of_purchase <- as.Date(purch$Date_of_purchase)
         
         if (country=="FR") { purch$Shopcode <- factor(purch$Shopcode)}
         else {purch$Banner_name <- factor(purch$Banner_name)}
         
         purch$Barcode <- factor(purch$Barcode)
         
         if (country == "NL") {
             purch$Promo <- ifelse(purch$Promo=="yes",1,0)
             purch$Promo <- as.logical(purch$Promo)
             purch$Purchase_method <- factor(purch$Purchase_method) 
         } else if (country== "GE") {
             colnames(purch)[8] <- "Quarter" }
         
         purch$Quarter <- factor(purch$Quarter)
         purch$Year <- as.numeric(substr(purch$Quarter,1,4))
         purch$Year <- factor(purch$Year)
} 




################################
# merging purchase and HH data
################################
if (country == "UK") {
} else { HH_purch <- merge(purch, HH, by=c("Panelist","Quarter")) } 




###############################################
# filter the household panels based on purchase
###############################################
# !!!
# household is out of the analysis if at any point, they do not buy anything 
# for a whole quarter

quarter_20182020 <- c("201801","201802", "201803","201804","201901","201902","201903",
                      "201904","202001","202002","202003","202004")
quarter_20182019 <- c("201801","201802", "201803","201804","201901","201902","201903","201904")
quarter_20192020 <- c("201901","201902","201903","201904","202001","202002","202003","202004")
quarter_2018 <- c("201801","201802", "201803","201804")
quarter_2019 <- c("201901","201902", "201903","201904")
quarter_2020 <- c("202001","202002", "202003","202004")

# function to determine whether panelist made purchase in every Quarter
filter_minpurchase_quarter <- function(df, quarterlist) {
  # create dataframe with all panelist ID and a flag 
  panelists <- data.frame(ID=unique(df$Panelist),filterOK=NA)
  # aggregate purchases up to panelist-quarter level
  df <- ddply(df,.(Panelist, Quarter), summarise, Total_unit_sales=sum(Total_unit_sales))
  # for every panelist --> make sure as many unique (because ddply) quarters as in list
  for (i in seq_along(panelists$ID)) {
    panelists$filterOK[i] <- ifelse(sum(df$Quarter[df$Panelist==panelists$ID[i]] %in% quarterlist)==length(quarterlist),1,0) }
  return(panelists)
}

# QUARTER - 2018-2020
if (country == "UK") {
  panelists_Q_2018 <- filter_minpurchase_quarter(purch2018, quarter_2018)
  panelists_Q_2019 <- filter_minpurchase_quarter(purch2019, quarter_2019)
  panelists_Q_2020 <- filter_minpurchase_quarter(purch2020, quarter_2020)
  panelists_Q_20182020 <- merge(panelists_Q_2018, panelists_Q_2019, by="ID")
  panelists_Q_20182020 <- merge(panelists_Q_20182020, panelists_Q_2020, by="ID")
  panelists_Q_20182020$filterOK <- ifelse(rowSums(panelists_Q_20182020[,2:4])==3,1,0)
} else { panelists_Q_20182020 <- filter_minpurchase_quarter(HH_purch, quarter_20182020) } 
table(panelists_Q_20182020$filterOK)




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

# QUARTER - 2018-2020
panelists_Q_20182020_HH <- filter_panelpres_quarter(HH, quarter_20182020)
table(panelists_Q_20182020_HH$filterOK)





#######################
# compare both filters
#######################

# ideally, only very few households who are excluded in purchase-based filter are included in panel-based filter
# these are the people who stopped purchasing (instead of left the panel)
sum(panelists_Q_20182020$ID[panelists_Q_20182020$filterOK==1] %in% panelists_Q_20182020_HH$ID[panelists_Q_20182020_HH$filterOK==1])
sum(panelists_Q_20182020$ID[panelists_Q_20182020$filterOK==0] %in% panelists_Q_20182020_HH$ID[panelists_Q_20182020_HH$filterOK==1])




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

save(panelists_keep_ID, file=paste(save_path,"/Household filters - general/",country,"_filtered_HH_ID.RData",sep=""))
# local
save(panelists_keep_ID, file=paste(project_path,"/R_files/Preparation/household filters/",country,"filtered_HH_ID.RData",sep=""))

