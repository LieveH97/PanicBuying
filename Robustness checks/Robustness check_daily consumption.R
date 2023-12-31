##############################################
###                                        ###
###  Preparing toilet paper barcode file   ###
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

### set path to save
if (Sys.info()["nodename"] == "GHUM-L-E939DCHK"){
  save_path <- "X:/Retailing_Group/PhD Lieve/Project 1"
}else if (Sys.info()["nodename"] == "GHUM-L-E0376K0Q") {
  save_path <- "X:/Retailing_Group/PhD Lieve/Project 1"
}

#load libraries - you have to install the packages before running the code!
list.of.packages <- c("plyr","tidyr","ggplot2", "tidyverse", "zoo", "readxl")

#Install package if they are not installes yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)


# create country list
country_list <- c("NL","BE","GE","FR","UK")





#########################
# Loading barcode data
#########################

# set country
country <- country_list[country_list=="NL"]

#read in barcode dataset
if (country == "GE") {
  barcode <- read_xlsx(paste(data_path,"/",country,"/barcode_new.xlsx",sep=""), col_types = "text")
} else { barcode <- read.csv(paste(data_path,"/",country,"/barcode.csv",sep="")) } 

#inspect the category names
barcode$Category_name <- factor(barcode$Category_name)

#based on country (language), only retain toilet paper category
if (country == "NL") {
toiletpaper <- barcode[barcode$Category_name=="toiletpapier"|barcode$Category_name=="toiletpaper K",]
} else if (country == "GE") {  toiletpaper <- barcode[barcode$Category_name=="TOILETTENPAPIER TROCKEN",]
} else if (country == "BE") {  toiletpaper <- barcode[barcode$Category_name=="toiletpapier"|barcode$Category_name=="toiletpapier K",]
} else if (country == "FR") {
} else if (country == "UK") {  toiletpaper <- barcode[barcode$Report_Sector=="Soft Toilet Rolls",]
}

rm(barcode)



###########################
# Inspecting barcode data
###########################
str(toiletpaper)
toiletpaper$Barcode <- factor(toiletpaper$Barcode)
#check whether each barcode only appears once
sort(table(toiletpaper$Barcode),decreasing=T)

toiletpaper$PL <- ifelse(toiletpaper$PL=="yes",1,0)

toiletpaper$Brand <- factor(toiletpaper$Brand)
toiletpaper$Sub_brand <- factor(toiletpaper$Sub_brand)
toiletpaper$BG_Category_name <- factor(toiletpaper$BG_Category_name)
toiletpaper$BG_Category_number <- factor(toiletpaper$BG_Category_number)

toiletpaper$Measurement_unit <- factor(toiletpaper$Measurement_unit)
table(toiletpaper$Measurement_unit)



####################################
# Creating sheet & ply variables
####################################
toiletpaper$ply <- NA
toiletpaper$sheet <- NA

toiletpaper$ply <- as.integer(sub(".*?(\\d+)\\s*-laags.*", "\\1", x=toiletpaper$Barcode_description))
toiletpaper[is.na(toiletpaper$ply),]      # one observation where description is NA -> no ply info



#######################
# load purchase data 
#######################

purch2018 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2018.csv",sep=""))
purch2019 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2019.csv",sep=""))
purch2020 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2020.csv",sep=""))
purch2018$Barcode <- factor(purch2018$Barcode)
purch2019$Barcode <- factor(purch2019$Barcode)
purch2020$Barcode <- factor(purch2020$Barcode)
#merge purchase data with toilet paper barcode
TP_purch <- merge(purch2018,toiletpaper,by="Barcode")
TP_purch <- rbind(TP_purch,merge(purch2019,toiletpaper,by="Barcode"))
TP_purch <- rbind(TP_purch, merge(purch2020,toiletpaper,by="Barcode"))
rm(list=c('purch2018','purch2019','purch2020'))
# only keep purchase data of households that passed the general filter
 # "each household should buy 'something' at least once every quarter"
load(file= paste(project_path,"/R_Files/Preparation/household filters/", country,"filtered_HH_ID.RData", sep=""))
TP_purch <- TP_purch[TP_purch$Panelist %in% panelists_keep_ID,]




#########################################
# inspecting toilet paper purchase data
#########################################
str(TP_purch)
TP_purch$Date_of_purchase <- as.Date(TP_purch$Date_of_purchase)
TP_purch$Banner_name <- factor(TP_purch$Banner_name)
TP_purch$Panelist <- factor(TP_purch$Panelist)

TP_purch$Promo <- ifelse(TP_purch$Promo=="yes",1,0)
TP_purch$Promo <- as.logical(TP_purch$Promo)

TP_purch$Quarter <- factor(TP_purch$Quarter)
TP_purch$Year <- as.numeric(substr(TP_purch$Quarter,1,4))
TP_purch$Year <- factor(TP_purch$Year)

TP_purch$HalfYear <- paste(TP_purch$Year, ifelse(substr(TP_purch$Quarter,5,6)=="01"|substr(TP_purch$Quarter,5,6)=="02","H1",""),
                           ifelse(substr(TP_purch$Quarter,5,6)=="03"|substr(TP_purch$Quarter,5,6)=="04","H2",""),sep="")

#international fixed calendar
load(paste(project_path,"/R_Files/Preparation/conversion_IFC.RData", sep=""))
TP_purch <- merge(TP_purch, IFC, by.x="Date_of_purchase",by.y="Date")

table(TP_purch$Total_unit_sales * TP_purch$Volume_per_unit == TP_purch$Total_volume_sales)

TP_purch$Total_volume_sales <- ifelse(TP_purch$Total_unit_sales * TP_purch$Volume_per_unit == TP_purch$Total_volume_sales, TP_purch$Total_volume_sales, TP_purch$Total_unit_sales * TP_purch$Volume_per_unit)




##############################
# cross-category panel filters 
##############################

### MINIMUM PURCHASE FILTER ###
# only keep households that make a toilet paper purchase at least once every year of 2018-2020

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
yearlist <- c("2018","2019","2020")
filter_minpurch <- filter_minpurchase_year(TP_purch,yearlist)
filter_minpurch <- filter_minpurch[filter_minpurch$filterOK==1,]
# apply filter to TP_purch dataset
TP_purch <- TP_purch[TP_purch$Panelist %in% filter_minpurch$ID,]



### INTERPURCHASE TIME FILTER ###
# exclude households whose maximum interpurchase time up to 1/1/2021 is larger than the mean + 2 standard deviations

#first aggregate up to Day-Household level
#TP_purch_agg_d <- ddply(TP_purch,.(Panelist,Date_of_purchase, Quarter, Year),
#                        summarise,tot_sheet = sum(tot_sheet))
TP_purch_agg_d <- ddply(TP_purch,.(Panelist,Date_of_purchase, Quarter, Year),
                        summarise,tot_rolls = sum(Total_volume_sales))
# calculate interpurchase time between every purchase
TP_purch_agg_d <- TP_purch_agg_d[order(TP_purch_agg_d$Panelist,TP_purch_agg_d$Date),]
TP_purch_agg_d$IPT<-do.call(c,by(TP_purch_agg_d$Date,TP_purch_agg_d$Panelist,function(x) c(NA,diff(x))))
# calculate each household's average interpurchase time

dsummary_2020 <- function(df) {
  df %>% group_by(flag) %>% summarize("Min."=min(max_IPT), "First Qu."=quantile(max_IPT,0.25),"Median"=median(max_IPT), "Mean"=mean(max_IPT),
                                      "Third Qu."= quantile(max_IPT,0.75), "Max."=max(max_IPT), "St.Dev"=sd(max_IPT)) }
TP_purch_agg_d$flag <- ifelse(TP_purch_agg_d$Date_of_purchase < as.Date("2020-01-01"), "before", "during/after")
# add December 31, 2019 as a zero-purchase in TP_purch_agg_d
# TP_purch_agg_d_cut <- rbind(TP_purch_agg_d,  cbind(Panelist=as.character(unique(TP_purch_agg_d$Panelist)), 
#                                                Date_of_purchase="2019-12-31", Quarter="201904", Year="2019", 
#                                                tot_sheet=0,IPT=NA,flag="before"))
TP_purch_agg_d_cut <- rbind(TP_purch_agg_d,  cbind(Panelist=as.character(unique(TP_purch_agg_d$Panelist)),
                                                   Date_of_purchase="2019-12-31", Quarter="201904", Year="2019",
                                                   tot_rolls=0,IPT=NA,flag="before"))
TP_purch_agg_d_cut <- TP_purch_agg_d_cut[order(TP_purch_agg_d_cut$Panelist,TP_purch_agg_d_cut$Date),]
TP_purch_agg_d_cut$IPT_cut <- do.call(c,by(TP_purch_agg_d_cut$Date,TP_purch_agg_d_cut$Panelist,function(x) c(NA,diff(x))))
# add flag
TP_purch_agg_d_cut$flag <- ifelse(TP_purch_agg_d_cut$Date_of_purchase < as.Date("2020-01-01"), "before2020", "2020")
max_IPT <-aggregate(IPT_cut ~ Panelist + flag, TP_purch_agg_d_cut, max)
colnames(max_IPT) <- c("Panelist", "flag", "max_IPT")
ggplot(max_IPT, aes(x=max_IPT, colour=factor(flag))) + geom_density()
dsummary_2020(max_IPT)
max_IPT_before2020 <- subset(max_IPT, flag=="before2020")
# filter: mean +/- 2 st.dev.
filter_IPT_2020_st <- max_IPT_before2020[max_IPT_before2020$max_IPT < mean(max_IPT_before2020$max_IPT)+2*sd(max_IPT_before2020$max_IPT), ]

panelists_keep_ID_cat <- as.vector(filter_IPT_2020_st$Panelist)
panelists_drop_ID_cat <- as.vector(unique(TP_purch$Panelist[!TP_purch$Panelist %in% filter_IPT_2020_st$Panelist]))




### APPLYING FILTERS TO PURCHASE DATA ###

TP_purch <- TP_purch[TP_purch$Panelist %in% panelists_keep_ID_cat,]
# only after analysis of kept vs. dropped! otherwise HH characteristics of dropped will be empty
# because first "only keep HH info of HH in TP_purch" -> TP_purch shouldn't be filtered yet!

TP_purch_agg_d <- TP_purch_agg_d[TP_purch_agg_d$Panelist %in% panelists_keep_ID_cat,]
TP_purch_agg_d_cut <- TP_purch_agg_d_cut[TP_purch_agg_d_cut$Panelist %in% panelists_keep_ID_cat,]

save(panelists_keep_ID_cat, file=paste(project_path,"/R_files/Preparation/toilet paper/",country,"_IPT_HH_filter.RData",sep=""))



##############################################
# aggregate purchase data to day + week level
##############################################

TP_purch_agg_y <- ddply(TP_purch, .(Panelist,Year), summarise, tot_rolls = sum(Total_volume_sales))






###########################################################
# ROBUSTNESS CHECK: average daily consumption per household
###########################################################

AvgDailyCons <- TP_purch_agg_d[,c("Panelist","Date_of_purchase","tot_rolls")]
names(AvgDailyCons) <- c("Panelist","Date","Q")

# households don't make a purchase each day --> add non-purchase days
# create dataframe of all dates-HH combos
HH_in_panel <- unique(TP_purch$Panelist)
all.dates <- seq(as.Date("2018-01-01"),as.Date("2020-12-31"),by="days")
temp <- data.frame(Panelist = rep(HH_in_panel, length(all.dates)), Date= rep(all.dates, each=length(HH_in_panel)))
# add dates of no purchases
AvgDailyCons <- merge(AvgDailyCons, temp, by=c("Panelist","Date"),all.y=T)
rm(temp)
# create year variable
AvgDailyCons$Year <- as.numeric(substr(AvgDailyCons$Date,1,4))
AvgDailyCons <- AvgDailyCons[with(AvgDailyCons,order(Panelist,Date)),]
row.names(AvgDailyCons) <- seq(1:nrow(AvgDailyCons))
AvgDailyCons$Q <- as.numeric(AvgDailyCons$Q)


# CBAR PER YEAR #
AvgDailyCons <- AvgDailyCons %>% group_by(Panelist, Year) %>% mutate(Cbar_2018 = ifelse(Year == 2018, sum(Q, na.rm = T)/365, NA),
                                                                     Cbar_2019 = ifelse(Year == 2019, sum(Q, na.rm = T)/365, NA),
                                                                     Cbar_2020 = ifelse(Year == 2020, sum(Q, na.rm = T)/366, NA))
AvgDailyCons$Cbar_year <- AvgDailyCons$Cbar_2018
AvgDailyCons$Cbar_year <- ifelse(is.na(AvgDailyCons$Cbar_year), AvgDailyCons$Cbar_2019, AvgDailyCons$Cbar_year)
AvgDailyCons$Cbar_year <- ifelse(is.na(AvgDailyCons$Cbar_year), AvgDailyCons$Cbar_2020, AvgDailyCons$Cbar_year)


# CBAR AS A CONSTANT #
AvgDailyCons <- AvgDailyCons %>% group_by(Panelist) %>% mutate(Cbar_cte = sum(Q, na.rm=T)/(365+365+366))


# CBAR AS A ROLLING AVERAGE OF 365 DAYS #
AvgDailyCons$Q[is.na(AvgDailyCons$Q)] <- 0
AvgDailyCons <- AvgDailyCons %>%
  group_by(Panelist) %>%
  mutate(Cbar_rolling365 = rollmean(Q, 365, fill = NA, align = "right")) %>%
  fill(Cbar_rolling365, .direction = "up")


# CBAR AS A ROLLING AVERAGE OF 4 MONTHS #
AvgDailyCons <- AvgDailyCons %>%
  group_by(Panelist) %>%
  mutate(Cbar_rolling122 = rollmean(Q, 122, fill = NA, align = "right")) %>%
  fill(Cbar_rolling122, .direction = "up")





#################################
# calculating household inventory
#################################

HH_Inv <- AvgDailyCons

# turn into list of lists
  # every HH is a list, containing: Date, Q, Cbar (year, cte, rolling365, rolling122), Inv
HH_Inv <- dlply(HH_Inv, .(Panelist), c)

# needed for some starting values based on IPT
avg_IPT_year <- aggregate(IPT~Panelist+Year, TP_purch_agg_d, mean)
avg_IPT_year_2019 <- subset(avg_IPT_year, Year=="2019", select= c("Panelist","IPT"))
avg_IPT_19 <- aggregate(IPT~Panelist, subset(TP_purch_agg_d, Year=="2019"), mean)
avg_IPT_1819 <- aggregate(IPT~Panelist, subset(TP_purch_agg_d, Year=="2018"|Year=="2019"), mean)
# max IPT calculated across 2018-2019, with hard cut at Dec 31,2019
max_IPT_before2020 <- aggregate(IPT_cut~Panelist, subset(TP_purch_agg_d_cut, Year=="2018"|Year=="2019"), max)

# calculate daily inventory
for (i in seq_along(HH_in_panel)) {
   ## max IPT of 2018-2019
   HH_Inv[[i]]$Inv_rolling365[1] <- HH_Inv[[i]]$Cbar_rolling365[1] * max_IPT_before2020$IPT_cut[max_IPT_before2020$Panelist==HH_Inv[[i]]$Panelist[1]]
   HH_Inv[[i]]$Inv_year[1] <- HH_Inv[[i]]$Cbar_year[1] * max_IPT_before2020$IPT_cut[max_IPT_before2020$Panelist==HH_Inv[[i]]$Panelist[1]]
   HH_Inv[[i]]$Inv_cte[1] <- HH_Inv[[i]]$Cbar_cte[1] * max_IPT_before2020$IPT_cut[max_IPT_before2020$Panelist==HH_Inv[[i]]$Panelist[1]]
   HH_Inv[[i]]$Inv_rolling122[1] <- HH_Inv[[i]]$Cbar_rolling122[1] * max_IPT_before2020$IPT_cut[max_IPT_before2020$Panelist==HH_Inv[[i]]$Panelist[1]]
   
      for (j in 2:1096) {
     HH_Inv[[i]]$Inv_rolling365[j] <- HH_Inv[[i]]$Inv_rolling365[j-1] + HH_Inv[[i]]$Q[j-1] - min(HH_Inv[[i]]$Inv_rolling365[j-1],HH_Inv[[i]]$Cbar_rolling365[j-1])
     HH_Inv[[i]]$Inv_year[j] <- HH_Inv[[i]]$Inv_year[j-1] + HH_Inv[[i]]$Q[j-1] - min(HH_Inv[[i]]$Inv_year[j-1],HH_Inv[[i]]$Cbar_year[j-1])
     HH_Inv[[i]]$Inv_cte[j] <- HH_Inv[[i]]$Inv_cte[j-1] + HH_Inv[[i]]$Q[j-1] - min(HH_Inv[[i]]$Inv_cte[j-1],HH_Inv[[i]]$Cbar_cte[j-1])
     HH_Inv[[i]]$Inv_rolling122[j] <- HH_Inv[[i]]$Inv_rolling122[j-1] + HH_Inv[[i]]$Q[j-1] - min(HH_Inv[[i]]$Inv_rolling122[j-1],HH_Inv[[i]]$Cbar_rolling122[j-1])
    }
 }

# unlist daily inventory 
Inv <- data.frame(ID=rep(names(HH_Inv), each=length(all.dates)), 
                  Date=rep(all.dates, length(panelists_keep_ID_cat)),
                  Inv_rolling365=unlist(lapply(HH_Inv[1:length(panelists_keep_ID_cat)],"[[",12 ), recursive=F),
                  Inv_year=unlist(lapply(HH_Inv[1:length(panelists_keep_ID_cat)],"[[",13 ), recursive=F),
                  Inv_cte=unlist(lapply(HH_Inv[1:length(panelists_keep_ID_cat)],"[[",14 ), recursive=F),
                  Inv_rolling122=unlist(lapply(HH_Inv[1:length(panelists_keep_ID_cat)],"[[",15 ), recursive=F))

  
  


###################################
# prepare inventory for final model
###################################

estimation.dates <- seq.Date(from=as.Date("2019-01-01"), to=as.Date("2020-04-30"), by="days")
Inv <- Inv[Inv$Date %in% estimation.dates,]

# mean centering inventory with household mean of 2019 and 2020
Inv <- Inv %>% group_by(ID) %>% mutate(meanINV_rolling365 = mean(Inv_rolling365),
                                       meanInv_year = mean(Inv_year),
                                       meanInv_cte = mean(Inv_cte),
                                       meanINV_rolling122 = mean(Inv_rolling122))
Inv$HHInv_rolling365 <- Inv$Inv_rolling365 - Inv$meanINV_rolling365
Inv$HHInv_year <- Inv$Inv_year - Inv$meanInv_year
Inv$HHInv_cte <- Inv$Inv_cte - Inv$meanInv_cte
Inv$HHInv_rolling122 <- Inv$Inv_rolling122 - Inv$meanINV_rolling122

# load Full Data from "inc_quant_toilet paper.R"
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))

FullData <- merge(FullData, Inv, by.x = c("Household", "Date"), by.y = c("ID", "Date"))


rm(list = c('avg_daily_cons', 'avg_IPT_1819', 'avg_IPT_19', 'avg_IPT_year', 'avg_IPT_year_2019', 'AvgDailyCons', 'filter_IPT_2020_st', 'filter_minpurch', 'HH_Inv',
            'IFC', 'Inv', 'max_IPT', 'max_IPT_before2020', 'toiletpaper', 'TP_purch', 'TP_purch_agg_d', 'TP_purch_agg_d_cut', 'TP_purch_agg_y', 'HH_in_panel',
            'panelists_drop_ID_cat', 'panelists_keep_ID', 'panelists_keep_ID_cat'))



  







######################################################
###              ESTIMATE FULL MODELS              ###
######################################################

# load the full dataset
load(paste(project_path,"/R_files/Robustness checks/FullData_with_different_inventories.RData",sep=""))


# final selection of relevant variables
variables.keep <- c("Total_volume_sales", "PromoIndex", "PanicIndex", "PromoSensitivity", "PLShare","BrandLoyalty", "Average_IPT", "Average_Q","PriceIndex", 
                    "Age", "HouseholdSize", "Income","Day_of_week", "Week", "Year","Household", "Date",
                    "HHInv_rolling365", "HHInv_cte", "HHInv_year", "HHInv_rolling122")  # robustness checks
FullData <- FullData[ , names(FullData) %in% variables.keep]
# when no purchase that day -> make unconditional quantity zero
FullData$unconditional_Total_volume_sales <- FullData$Total_volume_sales
FullData$unconditional_Total_volume_sales[is.na(FullData$unconditional_Total_volume_sales)] <- 0

two_stages_output_clean <- function(heck, name, keep_list){
  heckman_both_stages <- heck
  heckman_both_stages$param$index$betaO <- heck$param$index$betaS
  heckman_both_stages$param$index$betaS <- heck$param$index$betaO
  html_output <- stargazer(heck, heckman_both_stages, selection.equation = TRUE, dep.var.labels.include = F,        
                           column.labels = c("Purchase incidence", "(Conditional) Purchase Quantity"), type="html", keep=keep_list,
                           add.lines = list(c("Observations", heck$param$nObs, heck$param$N1), c("Year FE", "yes", "yes"), c("Week of year FE", "yes", "yes"),  c("Day of week FE", "yes", "yes")), initial.zero = F, aling = T,
                           out = paste(project_path,"/R_Files/Robustness checks/",name,".html", sep=""))
  return(html_output)
} 


#################################
# RQ1: PURCHASE - INCIDENCE MODEL
#################################

### TOBIT 2 - HECKMAN SELECTION MODEL ###
# main effects only, including controls
heckman_rolling365 <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                          + HHInv_rolling365 + Average_IPT 
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                          + HHInv_rolling365 + Average_Q   
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          data = FullData, method = "2step")
two_stages_output(heckman_rolling365, "RobustCheck_TwoStageModel_Inventory_rolling365")
rm(heckman_rolling365)


heckman_year <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                                + HHInv_year + Average_IPT 
                                + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                                outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                                + HHInv_year + Average_Q   
                                + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                                data = FullData, method = "2step")
two_stages_output(heckman_year, "RobustCheck_TwoStageModel_Inventory_year")
rm(heckman_year)


heckman_cte <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                          + HHInv_cte + Average_IPT 
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                          + HHInv_cte + Average_Q   
                          + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                          data = FullData, method = "2step")
two_stages_output(heckman_cte, "RobustCheck_TwoStageModel_Inventory_cte")
rm(heckman_cte)


heckman_rolling122 <- selection(selection = !is.na(Total_volume_sales) ~  PromoIndex + PanicIndex 
                                + HHInv_rolling122 + Average_IPT 
                                + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                                outcome =          Total_volume_sales  ~  PromoIndex + PanicIndex 
                                + HHInv_rolling122 + Average_Q   
                                + PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                                data = FullData, method = "2step")
# clean version: excluding time fixed effects from output
keep <- c("PromoIndex","PanicIndex","HHInv_rolling122","Average_IPT","Average_Q", "PriceIndex", "Age", "HouseholdSize", "Income")
two_stages_output_clean(heckman_rolling122, "RobustCheck_TwoStageModel_Inventory_rolling122")
rm(heckman_rolling122)





### REPLICATE WITH FIXED EFFECTS MODEL ###
fixed_effects_output <- function(fe, name) {
  html_output <- stargazer(fe, dep.var.labels.include = F, column.labels = "Unconditional Purchase Quantity", type = "html",
                           out = paste(project_path,"/R_Files/Robustness checks/",name,".html", sep=""))
  return(html_output)
}
logLik.plm <- function(object){
  out <- -plm::nobs(object) * log(2 * var(object$residuals) * pi)/2 - deviance(object)/(2 * var(object$residuals))
  
  attr(out,"df") <- nobs(object) - object$df.residual
  attr(out,"nobs") <- plm::nobs(summary(object))
  return(out)
}

# estimate fixed effects model with main effects only, including controls
fe_rolling365 <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                 HHInv_rolling365 + Average_IPT + Average_Q + 
                 PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
               data = FullData, index= c("Household", "Date"), model = "within")
# cluster standard errors by household
fe_rolling_365_clus <- coeftest(fe_rolling365, vcov = vcovHC, type = "HC1", cluster= "group")
# save output
fixed_effects_output(fe_rolling_365_clus, "RobustCheck_SimpleOLS_Inventory_rolling365")
logLik.plm(fe_rolling365)
rm(fe_rolling365)
rm(fe_rolling_365_clus)


fe_year <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                       HHInv_year + Average_IPT + Average_Q + 
                       PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                     data = FullData, index= c("Household", "Date"), model = "within")
# cluster standard errors by household
fe_year_clus <- coeftest(fe_year, vcov = vcovHC, type = "HC1", cluster= "group")
# save output
fixed_effects_output(fe_year_clus, "RobustCheck_SimpleOLS_Inventory_year")

rm(fe_year)
rm(fe_year_clus)


fe_cte <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                 HHInv_cte + Average_IPT + Average_Q + 
                 PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
               data = FullData, index= c("Household", "Date"), model = "within")
# cluster standard errors by household
fe_cte_clus <- coeftest(fe_cte, vcov = vcovHC, type = "HC1", cluster= "group")
# save output
fixed_effects_output(fe_cte_clus, "RobustCheck_SimpleOLS_Inventory_cte")

rm(fe_cte)
rm(fe_cte_clus)


fe_rolling122 <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                       HHInv_rolling122 + Average_IPT + Average_Q + 
                       PriceIndex + Age + HouseholdSize + Income + Day_of_week + Week + Year,
                     data = FullData, index= c("Household", "Date"), model = "within")
# cluster standard errors by household
fe_rolling_122_clus <- coeftest(fe_rolling122, vcov = vcovHC, type = "HC1", cluster= "group")
# save output
fixed_effects_output(fe_rolling_122_clus, "RobustCheck_SimpleOLS_Inventory_rolling122")
rm(fe_rolling122)
rm(fe_rolling_122_clus)








##############################################
# RQ2: FIXED EFFECTS MODEL WITH ALL MODERATORS
##############################################

### FIXED EFFECTS MODEL WITH DEMOGRAPHICS AS MODERATORS

fe_rolling365 <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                # gain-seeking moderators
                PromoIndex*PromoSensitivity + PanicIndex*PromoSensitivity + 
                PromoIndex*PLShare + PanicIndex*PLShare + 
                PromoIndex*BrandLoyalty + PanicIndex*BrandLoyalty + 
                # loss avoidance moderators
                PromoIndex*Average_IPT + PanicIndex*Average_IPT + 
                PromoIndex*Average_Q + PanicIndex*Average_Q + 
                PromoIndex*HHInv_rolling365 + PanicIndex*HHInv_rolling365 +
                # demographics
                PromoIndex*Age + PanicIndex*Age + 
                PromoIndex*HouseholdSize + PanicIndex*HouseholdSize +
                PromoIndex*Income + PanicIndex*Income +
                # controls
                PriceIndex + Day_of_week + Week + Year,
              data = FullData, index= c("Household", "Date"), model = "within")
# variance-covariance matrix with clustered standard errors
vcov_FE_clus <- vcovHC(fe_rolling365, type = "HC1", cluster = "group")
# add clustered standard errors to model output
fe_rolling365_clus <- coeftest(fe_rolling365, vcov = vcov_FE_clus)
# save output
fixed_effects_output(fe_rolling365_clus, "RobustCheck_FullOLS_Inventory_rolling365")
rm(fe_rolling365)
rm(fe_rolling365_clus)



fe_year <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                       # gain-seeking moderators
                       PromoIndex*PromoSensitivity + PanicIndex*PromoSensitivity + 
                       PromoIndex*PLShare + PanicIndex*PLShare + 
                       PromoIndex*BrandLoyalty + PanicIndex*BrandLoyalty + 
                       # loss avoidance moderators
                       PromoIndex*Average_IPT + PanicIndex*Average_IPT + 
                       PromoIndex*Average_Q + PanicIndex*Average_Q + 
                       PromoIndex*HHInv_year + PanicIndex*HHInv_year +
                       # demographics
                       PromoIndex*Age + PanicIndex*Age + 
                       PromoIndex*HouseholdSize + PanicIndex*HouseholdSize +
                       PromoIndex*Income + PanicIndex*Income +
                       # controls
                       PriceIndex + Day_of_week + Week + Year,
                     data = FullData, index= c("Household", "Date"), model = "within")
# variance-covariance matrix with clustered standard errors
vcov_FE_clus <- vcovHC(fe_year, type = "HC1", cluster = "group")
# add clustered standard errors to model output
fe_year_clus <- coeftest(fe_year, vcov = vcov_FE_clus)
# save output
fixed_effects_output(fe_year_clus, "RobustCheck_FullOLS_Inventory_year")
rm(fe_year)
rm(fe_year_clus)


fe_cte <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                 # gain-seeking moderators
                 PromoIndex*PromoSensitivity + PanicIndex*PromoSensitivity + 
                 PromoIndex*PLShare + PanicIndex*PLShare + 
                 PromoIndex*BrandLoyalty + PanicIndex*BrandLoyalty + 
                 # loss avoidance moderators
                 PromoIndex*Average_IPT + PanicIndex*Average_IPT + 
                 PromoIndex*Average_Q + PanicIndex*Average_Q + 
                 PromoIndex*HHInv_cte + PanicIndex*HHInv_cte +
                 # demographics
                 PromoIndex*Age + PanicIndex*Age + 
                 PromoIndex*HouseholdSize + PanicIndex*HouseholdSize +
                 PromoIndex*Income + PanicIndex*Income +
                 # controls
                 PriceIndex + Day_of_week + Week + Year,
               data = FullData, index= c("Household", "Date"), model = "within")
# variance-covariance matrix with clustered standard errors
vcov_FE_clus <- vcovHC(fe_cte, type = "HC1", cluster = "group")
# add clustered standard errors to model output
fe_cte_clus <- coeftest(fe_cte, vcov = vcov_FE_clus)
# save output
fixed_effects_output(fe_cte_clus, "RobustCheck_FullOLS_Inventory_cte")
rm(fe_cte)
rm(fe_cte_clus)



fe_rolling122 <- plm(unconditional_Total_volume_sales ~  PromoIndex + PanicIndex + 
                       # gain-seeking moderators
                       PromoIndex*PromoSensitivity + PanicIndex*PromoSensitivity + 
                       PromoIndex*PLShare + PanicIndex*PLShare + 
                       PromoIndex*BrandLoyalty + PanicIndex*BrandLoyalty + 
                       # loss avoidance moderators
                       PromoIndex*Average_IPT + PanicIndex*Average_IPT + 
                       PromoIndex*Average_Q + PanicIndex*Average_Q + 
                       PromoIndex*HHInv_rolling122 + PanicIndex*HHInv_rolling122 +
                       # demographics
                       PromoIndex*Age + PanicIndex*Age + 
                       PromoIndex*HouseholdSize + PanicIndex*HouseholdSize +
                       PromoIndex*Income + PanicIndex*Income +
                       # controls
                       PriceIndex + Day_of_week + Week + Year,
                     data = FullData, index= c("Household", "Date"), model = "within")
# variance-covariance matrix with clustered standard errors
vcov_FE_clus <- vcovHC(fe_rolling122, type = "HC1", cluster = "group")
# add clustered standard errors to model output
fe_rolling122_clus <- coeftest(fe_rolling122, vcov = vcov_FE_clus)
# save output
fixed_effects_output(fe_rolling122_clus, "RobustCheck_FullOLS_Inventory_rolling122")
rm(fe_rolling122)
rm(fe_rolling122_clus)











######################################################
# SAVE DATASET WITH DIFFERENT INVENTORY CALCULATIONS #
######################################################


save(FullData, file=paste(project_path,"/R_files/Robustness checks/FullData_with_different_inventories.RData",sep=""))
