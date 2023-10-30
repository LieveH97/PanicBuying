##############################################
###                                        ###
###     Preparing pasta: inventory DV      ###
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
list.of.packages <- c("plyr","tidyr","ggplot2", "tidyverse", "zoo")

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


# if working with server: better to prepare toilet paper dataset once and saving it as RData file -> faster to load from Xdrive
#load(file=paste("./R_Files/Preparation/toilet paper/prepared_toiletpaper_",country,".RData",sep=""))


# otherwise:
#read in barcode dataset
barcode <- read.csv(paste(data_path,"/",country,"/barcode.csv",sep=""))

#inspect the category names
barcode$Category_name <- factor(barcode$Category_name)

#based on country (language), only retain pasta categories
if (country == "NL") {
pasta <- barcode[barcode$Category_name=="pasta gevuld"|barcode$Category_name=="pasta lasagne"
                      |barcode$Category_name=="pasta macaroni"|barcode$Category_name=="pasta mie-mihoen"
                      |barcode$Category_name=="pasta spaghetti"|barcode$Category_name=="pasta vorm anders",]
pasta <- pasta[grepl("geen koelvak\\-diepvr",pasta$Barcode_description),]
} else if (country == "GE") {  
} else if (country == "BE") {
} else if (country == "FR") {
} else if (country == "UK") {
}

rm(barcode)



###########################
# Inspecting barcode data
###########################
str(pasta)

pasta$Barcode <- factor(pasta$Barcode)
#check whether each barcode only appears once
sort(table(pasta$Barcode),decreasing=T)

pasta$PL <- ifelse(pasta$PL=="yes",1,0)

pasta$Brand <- factor(pasta$Brand)
pasta$Sub_brand <- factor(pasta$Sub_brand)
pasta$BG_Category_name <- factor(pasta$BG_Category_name)
pasta$BG_Category_number <- factor(pasta$BG_Category_number)

pasta$Measurement_unit <- factor(pasta$Measurement_unit)
table(pasta$Measurement_unit)



#######################
# load purchase data 
#######################

if (location == "server") {
  #load purchase dataset of filtered HHs
  load(paste(save_path,"/filteredPurchdata_", country,".RData", sep=""))
  purch$Barcode <- factor(purch$Barcode)
  pasta_purch <- merge(purch, pasta, by="Barcode")
  rm(purch)
} else if (location == "local") {
  if (country == "NL") {
    purch2018 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2018.csv",sep=""))
    purch2019 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2019.csv",sep=""))
    purch2020 <- read.csv(paste(data_path,"/",country,"/purchase_promo_2020.csv",sep=""))
  } else if (country == "GE") {
  } else if (country == "BE") {
  } else if (country == "FR") {
  } else if (country == "UK") {
  }
  purch2018$Barcode <- factor(purch2018$Barcode)
  purch2019$Barcode <- factor(purch2019$Barcode)
  purch2020$Barcode <- factor(purch2020$Barcode)
  #merge purchase data with pasta barcode
  pasta_purch <- merge(purch2018,pasta,by="Barcode")
  pasta_purch <- rbind(pasta_purch,merge(purch2019,pasta,by="Barcode"))
  pasta_purch <- rbind(pasta_purch, merge(purch2020,pasta,by="Barcode"))
  rm(list=c('purch2018','purch2019','purch2020'))
  # only keep purchase data of households that passed the general filter
       # "each household should buy 'something' at least once every quarter"
  load(file= paste(project_path,"/R_Files/Preparation/household filters/", country,"filtered_HH_ID.RData", sep=""))
  pasta_purch <- pasta_purch[pasta_purch$Panelist %in% panelists_keep_ID,]
}





################################
# inspecting pasta purchase data
################################
str(pasta_purch)
pasta_purch$Date_of_purchase <- as.Date(pasta_purch$Date_of_purchase)
pasta_purch$Banner_name <- factor(pasta_purch$Banner_name)
pasta_purch$Panelist <- factor(pasta_purch$Panelist)

pasta_purch$Promo <- ifelse(pasta_purch$Promo=="yes",1,0)
pasta_purch$Promo <- as.logical(pasta_purch$Promo)

pasta_purch$Year <- as.numeric(substr(pasta_purch$Quarter,1,4))
pasta_purch$Year <- factor(pasta_purch$Year)

pasta_purch$HalfYear <- paste(pasta_purch$Year, ifelse(substr(pasta_purch$Quarter,5,6)=="01"|substr(pasta_purch$Quarter,5,6)=="02","H1",""),
                           ifelse(substr(pasta_purch$Quarter,5,6)=="03"|substr(pasta_purch$Quarter,5,6)=="04","H2",""),sep="")

#13-month calendar conversion
load(paste(project_path,"/R_Files/Preparation/conversion_IFC.RData", sep=""))
pasta_purch <- merge(pasta_purch, IFC, by.x="Date_of_purchase",by.y="Date")







###############################
# filtering pasta purchase data 
###############################

## after applying cross-category filter
   # only HHs that buy something (doesn't matter which category) once every quarter of 2018-2020
   # otherwise not a regular grocery shopper
length(unique(pasta_purch$Panelist))

## min purchase filter
   # only HHs that made a pasta purchase every year between 2018-2020
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
filter_minpurch <- filter_minpurchase_year(pasta_purch,yearlist)
filter_minpurch <- filter_minpurch[filter_minpurch$filterOK==1,]
# apply filter to pasta_purch dataset
pasta_purch <- pasta_purch[pasta_purch$Panelist %in% filter_minpurch$ID,]
length(unique(pasta_purch$Panelist))


## Interpurchase time filter
  # first aggregate up to Day-Household level
pasta_purch_agg_d <- ddply(pasta_purch,.(Panelist,Date_of_purchase, Quarter, Year),
                        summarise,tot_grams = sum(Total_volume_sales))
  # calculate interpurchase time between every purchase
pasta_purch_agg_d <- pasta_purch_agg_d[order(pasta_purch_agg_d$Panelist,pasta_purch_agg_d$Date),]
pasta_purch_agg_d$IPT<-do.call(c,by(pasta_purch_agg_d$Date,pasta_purch_agg_d$Panelist,function(x) c(NA,diff(x))))

#before vs after 2020 (making hard cut in IPT)
dsummary_2020 <- function(df) {
  df %>% group_by(flag) %>% summarize("Min."=min(max_IPT), "First Qu."=quantile(max_IPT,0.25),"Median"=median(max_IPT), "Mean"=mean(max_IPT),
                                      "Third Qu."= quantile(max_IPT,0.75), "Max."=max(max_IPT), "St.Dev"=sd(max_IPT)) }
# add December 31, 2019 as a zero-purchase in pasta_purch_agg_d
pasta_purch_agg_d_cut <- rbind(pasta_purch_agg_d,  cbind(Panelist=as.character(unique(pasta_purch_agg_d$Panelist)), 
                                               Date_of_purchase="2019-12-31", Quarter="201904", Year="2019", 
                                               tot_grams=0,IPT=NA))
pasta_purch_agg_d_cut <- pasta_purch_agg_d_cut[order(pasta_purch_agg_d_cut$Panelist,pasta_purch_agg_d_cut$Date),]
pasta_purch_agg_d_cut$IPT_cut <- do.call(c,by(pasta_purch_agg_d_cut$Date,pasta_purch_agg_d_cut$Panelist,function(x) c(NA,diff(x))))
# add flag
pasta_purch_agg_d_cut$flag <- ifelse(pasta_purch_agg_d_cut$Date_of_purchase < as.Date("2020-01-01"), "before2020", "2020")
max_IPT <-aggregate(IPT_cut ~ Panelist + flag, pasta_purch_agg_d_cut, max)
colnames(max_IPT) <- c("Panelist", "flag", "max_IPT")
ggplot(max_IPT, aes(x=max_IPT, colour=factor(flag))) + geom_density()
dsummary_2020(max_IPT)
max_IPT_before2020 <- subset(max_IPT, flag=="before2020")
# filter: mean +/- 2 st.dev.
filter_IPT_2020_st <- max_IPT_before2020[max_IPT_before2020$max_IPT < mean(max_IPT_before2020$max_IPT)+2*sd(max_IPT_before2020$max_IPT), ]
# investigate during/after period after applying filter
ggplot(max_IPT[max_IPT$Panelist %in% filter_IPT_2020_st$Panelist,], aes(x=max_IPT, color=factor(flag))) + geom_density()
dsummary_2020(max_IPT[max_IPT$Panelist %in% filter_IPT_2020_st$Panelist,])
# apply filter
panelists_keep_ID_cat <- as.vector(filter_IPT_2020_st$Panelist)
panelists_drop_ID_cat <- as.vector(unique(pasta_purch$Panelist[!pasta_purch$Panelist %in% filter_IPT_2020_st$Panelist]))






#####################################################
# investigate characteristics of kept vs. dropped HH
#####################################################

# loading HH data to get characteristics
if (location == "server") {
  load(paste(save_path,"/filteredHHdata_",country,".RData", sep=""))
} else if (location == "local") {
  HH <- read.csv(paste(data_path, "/", country, "/panelist.csv", sep=""))
}

     
# only keep HH info of panelists in TP dataset
HH <- HH[HH$Panelist %in% unique(pasta_purch$Panelist),]

HH_keep <- HH[HH$Panelist %in% panelists_keep_ID_cat,]
HH_drop <- HH[HH$Panelist %in% panelists_drop_ID_cat,]

## compare characteristics of the households we exclude with those we keep
#comparing age of households we exclude or keep
age_keep <- prop.table(table(HH_keep$Age))
age_drop <- prop.table(table(HH_drop$Age))
age_OG <- prop.table(table(HH$Age))
age_compare <- round(rbind(age_OG, age_keep, age_drop),5)
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

rm(list=ls(pattern="_keep$"))
rm(list=ls(pattern="_drop$"))
rm(list=ls(pattern="_OG$"))
rm(list=ls(pattern="_compare$"))




################################
# apply filter to purchase data
################################

pasta_purch <- pasta_purch[pasta_purch$Panelist %in% panelists_keep_ID_cat,]
# only after analysis of kept vs. dropped! otherwise HH characteristics of dropped will be empty
# because first "only keep HH info of HH in pasta_purch" -> pasta_purch shouldn't be filtered yet!
length(unique(pasta_purch$Panelist))
pasta_purch_agg_d <- pasta_purch_agg_d[pasta_purch_agg_d$Panelist %in% panelists_keep_ID_cat,]
pasta_purch_agg_d_cut <- pasta_purch_agg_d_cut[pasta_purch_agg_d_cut$Panelist %in% panelists_keep_ID_cat,]





##################################
# descriptives of filtered data 
##################################


# PACKAGE SIZES
round(prop.table(table(pasta_purch$Volume_per_unit)),4)

# AVERAGE PRICES
pasta_purch$AvgPrice_pack <- pasta_purch$Total_value_sales / pasta_purch$Total_unit_sales * 0.01
pasta_purch$AvgPrice_gram <- pasta_purch$Total_value_sales / pasta_purch$Total_volume_sales * 0.01
pasta_purch$AvgPrice_stpack <- pasta_purch$AvgPrice_gram * 500

# average price of pack, per pasta type (packs can be of different sizes (in grams)!)
ddply(pasta_purch, "Category_name", summarise, avg_price_pack=mean(AvgPrice_pack))
# average price per gram, per pasta type
ddply(pasta_purch, "Category_name", summarise, avg_price_gram = mean(AvgPrice_gram))
# average price of 500g-standardized pack, per pasta type
ddply(pasta_purch, "Category_name", summarise, avg_price_stpack=mean(AvgPrice_stpack))

# PASTA TYPE
# SKUs per pasta type
ggplot(pasta, aes(x=factor(Category_name))) + geom_bar(position="dodge", show.legend=F)
# calculating market shares of "pasta type" per year
type_MS <- ddply(pasta_purch, .(Year, Category_name), summarise, Total_unit_sales = sum(Total_unit_sales),
                 Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
year_sum <- ddply(pasta_purch, .(Year), summarise, Total_unit_sales = sum(Total_unit_sales),
                  Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
year_sum <- (do.call("rbind", replicate(length(unique(pasta_purch$Category_name)), year_sum, simplify = FALSE)))
year_sum <- year_sum[order(year_sum$Year),]
type_MS[,3:5] <- type_MS[,3:5] / year_sum[,2:4]
type_MS[,3:5] <- round(type_MS[,3:5],4)
# total unit market shares
plot_type_MS_unit <- ggplot(type_MS, aes(x=Year,y=Total_unit_sales,fill=factor(Category_name))) + geom_bar(position="dodge", stat="identity")
type_MS[order(type_MS$Category_name),c("Category_name","Year","Total_unit_sales")]
# total volume market shares
plot_type_MS_volume <- ggplot(type_MS, aes(x=Year,y=Total_volume_sales,fill=factor(Category_name))) + geom_bar(position="dodge", stat="identity")
type_MS[order(type_MS$Category_name),c("Category_name","Year","Total_volume_sales")]
# total value market shares
plot_type_MS_value <- ggplot(type_MS, aes(x=Year,y=Total_value_sales,fill=factor(Category_name))) + geom_bar(position="dodge", stat="identity")
type_MS[order(type_MS$Category_name),c("Category_name","Year","Total_value_sales")]



# PRIVATE LABEL
ggplot(pasta, aes(x=factor(PL))) + geom_bar(position="dodge", show.legend=F)
# calculating market shares of "PL type" per year
PL_MS <- ddply(pasta_purch, .(Year, PL), summarise, Total_unit_sales = sum(Total_unit_sales),
               Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
year_sum <- ddply(pasta_purch, .(Year), summarise, Total_unit_sales = sum(Total_unit_sales),
                  Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
year_sum <- (do.call("rbind", replicate(length(unique(pasta_purch$PL)), year_sum, simplify = FALSE)))
year_sum <- year_sum[order(year_sum$Year),]
PL_MS[,3:5] <- PL_MS[,3:5] / year_sum[,2:4]
# total unit market shares
plot_PL_MS_unit <- ggplot(PL_MS, aes(x=Year,y=Total_unit_sales,fill=factor(PL))) + geom_bar(position="dodge", stat="identity")
PL_MS[order(PL_MS$PL),c("PL","Year","Total_unit_sales")]
# total volume market shares
plot_PL_MS_volume <- ggplot(PL_MS, aes(x=Year,y=Total_volume_sales,fill=factor(PL))) + geom_bar(position="dodge", stat="identity")
PL_MS[order(PL_MS$PL),c("PL","Year","Total_volume_sales")]
# total value market shares
plot_PL_MS_value <- ggplot(PL_MS, aes(x=Year,y=Total_value_sales,fill=factor(PL))) + geom_bar(position="dodge", stat="identity")
PL_MS[order(PL_MS$PL),c("PL","Year","Total_value_sales")]

rm(list=ls(pattern="^plot"))
rm(list=ls(pattern="_MS"))
rm(year_sum)



# TOTAL QUANTITY/VALUE/VOLUME SOLD + AVERAGE PRICE
# PER DAY
tot_Q <- ddply(pasta_purch, .(Date_of_purchase, Year), summarize, Tot_unit_sales = sum(Total_unit_sales),
               Tot_volume_sales=sum(Total_volume_sales), Tot_value_sales=sum(Total_value_sales))
tot_Q$avgP <- tot_Q$Tot_value_sales / tot_Q$Tot_volume_sales
tot_Q$avgPst <- tot_Q$avgP * 500
tot_Q$Date_of_purchase <- sub(pattern="2018-","",x=tot_Q$Date_of_purchase)
tot_Q$Date_of_purchase <- sub(pattern="2019-","",x=tot_Q$Date_of_purchase)
tot_Q$Date_of_purchase <- sub(pattern="2020-","",x=tot_Q$Date_of_purchase)
ggplot(tot_Q, aes(x=Date_of_purchase, y=Tot_unit_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Date_of_purchase, y=Tot_volume_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Date_of_purchase, y=Tot_value_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Date_of_purchase, y=avgPst, group=Year)) + geom_line(aes(colour=factor(Year))) + labs(y="average price for a 500g standardized pack in cents")
tot_Q %>% 
  group_by(Year) %>% 
  summarize("average price per gram"=mean(avgP), "average price per 500g"=mean(avgPst))
rm(tot_Q)

# PER WEEK
tot_Q <- ddply(pasta_purch, .(Week, Year), summarize, Tot_unit_sales = sum(Total_unit_sales),
               Tot_volume_sales=sum(Total_volume_sales), Tot_value_sales=sum(Total_value_sales))
tot_Q$avgP <- tot_Q$Tot_value_sales / tot_Q$Tot_volume_sales
ggplot(tot_Q, aes(x=Week, y=Tot_unit_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Week, y=Tot_volume_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Week, y=Tot_value_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Week, y=avgP*500, group=Year)) + geom_line(aes(colour=factor(Year))) + labs(y="average price for a 500g standardized pack in cents")
rm(tot_Q)

# PER IFC WEEK
tot_Q <- ddply(pasta_purch[!is.na(pasta_purch$Week_IFC),], .(Week_IFC, Year), summarize, Tot_unit_sales = sum(Total_unit_sales),
               Tot_volume_sales=sum(Total_volume_sales), Tot_value_sales=sum(Total_value_sales))
tot_Q$avgP <- tot_Q$Tot_value_sales / tot_Q$Tot_volume_sales
ggplot(tot_Q, aes(x=Week_IFC, y=Tot_unit_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Week_IFC, y=Tot_volume_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Week_IFC, y=Tot_value_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Week_IFC, y=avgP*500, group=Year)) + geom_line(aes(colour=factor(Year))) + labs(y="average price for a 500g standardized pack in cents")
rm(tot_Q)



# SALES VOLUME PER HOUSEHOLD OVER TIME
# PER DAY
Q_per_HH <- ddply(pasta_purch, .(Date_of_purchase, Year), summarize, Tot_volume_sales=sum(Total_volume_sales), HH_buying = length(Date_of_purchase))
Q_per_HH$Q_HH <- Q_per_HH$Tot_volume_sales / Q_per_HH$HH_buying
Q_per_HH$Date_of_purchase <- sub(pattern="2018-","",x=Q_per_HH$Date_of_purchase)
Q_per_HH$Date_of_purchase <- sub(pattern="2019-","",x=Q_per_HH$Date_of_purchase)
Q_per_HH$Date_of_purchase <- sub(pattern="2020-","",x=Q_per_HH$Date_of_purchase)
ggplot(Q_per_HH, aes(x=Date_of_purchase, y=Q_HH/500, group=Year)) + geom_line(aes(colour=factor(Year))) + labs(y="average Q per Household (in standardized packs of 500g)")
ggplot(Q_per_HH, aes(x=Date_of_purchase, y=HH_buying, group=Year)) + geom_line(aes(colour=factor(Year))) + labs("number of households buying")
rm (Q_per_HH)

# PER WEEK
Q_per_HH <- ddply(pasta_purch, .(Week, Year), summarize, Tot_volume_sales=sum(Total_volume_sales), HH_buying = length(Week))
Q_per_HH$Q_HH <- Q_per_HH$Tot_volume_sales / Q_per_HH$HH_buying
ggplot(Q_per_HH, aes(x=Week, y=Q_HH, group=Year)) + geom_line(aes(colour=factor(Year))) + labs(y="average Q per Household")
rm (Q_per_HH)

# PER IFC WEEK
Q_per_HH <- ddply(pasta_purch[!is.na(pasta_purch$Week_IFC),], .(Week_IFC, Year), summarize, Tot_volume_sales=sum(Total_volume_sales), HH_buying = length(Week_IFC))
Q_per_HH$Q_HH <- Q_per_HH$Tot_volume_sales / Q_per_HH$HH_buying
ggplot(Q_per_HH, aes(x=Week_IFC, y=Q_HH/500, group=Year)) + geom_line(aes(colour=factor(Year))) + labs(y="average Q per Household")
#number of HH buying
ggplot(Q_per_HH, aes(x=Week_IFC, y=HH_buying, group=Year)) + geom_line(aes(colour=factor(Year))) + labs("number of households buying")
rm (Q_per_HH)


##############################################
# aggregate purchase data to day + week level
##############################################

#pasta_purch_agg_d <- ddply(pasta_purch,.(Panelist,Date_of_purchase, Quarter, Year),
#                        summarise,tot_sheet = sum(tot_sheet),
#                        tot_sheet_PL=sum(tot_sheet_PL),tot_sheet_noPL=sum(tot_sheet_noPL), 
#                        tot_sheet_ply1 = sum(tot_sheet_ply1),tot_sheet_ply2 = sum(tot_sheet_ply2),
#                        tot_sheet_ply3 = sum(tot_sheet_ply3),tot_sheet_ply4 = sum(tot_sheet_ply4),
#                        tot_sheet_ply5 = sum(tot_sheet_ply5))

#pasta_purch_agg_w <- ddply(pasta_purch,.(Panelist,Week, Quarter, Year),
#                        summarise, tot_sheet = sum(tot_sheet),
#                        tot_sheet_PL=sum(tot_sheet_PL),tot_sheet_noPL=sum(tot_sheet_noPL), 
#                        tot_sheet_ply1 = sum(tot_sheet_ply1),tot_sheet_ply2 = sum(tot_sheet_ply2),
#                        tot_sheet_ply3 = sum(tot_sheet_ply3),tot_sheet_ply4 = sum(tot_sheet_ply4),
#                        tot_sheet_ply5 = sum(tot_sheet_ply5))

#pasta_purch_agg_q <- ddply(pasta_purch,.(Panelist, Quarter, Year),
#                        summarise, tot_sheet = sum(tot_sheet),
#                        tot_sheet_PL=sum(tot_sheet_PL),tot_sheet_noPL=sum(tot_sheet_noPL), 
#                        tot_sheet_ply1 = sum(tot_sheet_ply1),tot_sheet_ply2 = sum(tot_sheet_ply2),
#                        tot_sheet_ply3 = sum(tot_sheet_ply3),tot_sheet_ply4 = sum(tot_sheet_ply4),
#                        tot_sheet_ply5 = sum(tot_sheet_ply5))

pasta_purch_agg_y <- ddply(pasta_purch, .(Panelist,Year), summarise, tot_grams = sum(Total_volume_sales))




#######################################################################
# average time since previous purchase (averaged across HHs) over time
#######################################################################

PrevPurch <- aggregate(IPT ~ Date_of_purchase, pasta_purch_agg_d, mean)

ggplot(PrevPurch, aes(x=Date_of_purchase, y=IPT)) + geom_line() + geom_vline(xintercept=as.Date("2020-03-12"), col="blue") + geom_hline(yintercept = mean(PrevPurch$IPT), col= "red")+ ylab("Time since previous purchase (in days)")

excl_init <- seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by="days")
ggplot(PrevPurch[PrevPurch$Date_of_purchase %in% excl_init,],aes(x=Date_of_purchase, y=IPT)) + geom_line() + geom_vline(xintercept=as.Date("2020-03-12"), col="blue") + ylab("Time since previous purchase (in days)")

rm(PrevPurch)




##########################################
# average daily consumption per household
##########################################


# PER YEAR #
# investigate whether consumption increases/decreases/remains constant during 2020 compared to previous years
Cbar_year <- data.frame(HH_ID = pasta_purch_agg_y$Panelist, Year=pasta_purch_agg_y$Year,
                             Cbar = pasta_purch_agg_y$tot_grams / c(365,365,366))
ggplot(Cbar_year, aes(x=Cbar, group=Year)) + geom_density(aes(colour=factor(Year))) + xlab("daily consumption of pasta (in grams)")
Cbar_year %>% 
  group_by(Year) %>% 
  summarize("Min."=min(Cbar), "First Qu."=quantile(Cbar, 0.25), "Median"=median(Cbar),
            "Mean"=mean(Cbar),"Third Qu."=quantile(Cbar,0.75), "Max."=max(Cbar), "St.Dev."=sd(Cbar))




# BEFORE VS DURING HOARDING
pasta_purch_agg_h <- pasta_purch[pasta_purch$Date_of_purchase > as.Date("2019-12-31"),]
pasta_purch_agg_h$flag <- ifelse(pasta_purch_agg_h$Date_of_purchase < as.Date("2020-03-12"), "before", "hoard")
pasta_purch_agg_h <- ddply(pasta_purch_agg_h, .(Panelist, flag), summarise, tot_grams = sum(Total_volume_sales))
Cbar_hoard <- data.frame(HH_ID = pasta_purch_agg_h$Panelist, flag= pasta_purch_agg_h$flag,
                         Cbar = pasta_purch_agg_h$tot_grams / c(71,295))
ggplot(Cbar_hoard, aes(x=Cbar, group= flag)) + geom_density(aes(colour=factor(flag)))
Cbar_hoard %>% 
  group_by(flag) %>% 
  summarize("Min."=min(Cbar), "First Qu."=quantile(Cbar, 0.25), "Median"=median(Cbar),
            "Mean"=mean(Cbar),"Third Qu."=quantile(Cbar,0.75), "Max."=max(Cbar), "St.Dev."=sd(Cbar))




# calculate Cbar to use in INV calculations
# create dataframe with all dates (not only ones with purchases)
AvgDailyCons <- pasta_purch_agg_d[,c("Panelist","Date_of_purchase","tot_grams")]
names(AvgDailyCons) <- c("Panelist","Date","Q")
# create dataframe of all dates-HH combos
HH_in_panel <- unique(pasta_purch$Panelist)
all.dates <- seq(as.Date("2018-01-01"),as.Date("2020-12-31"),by="days")
temp <- data.frame(Panelist = rep(HH_in_panel, length(all.dates)), Date= rep(all.dates, each=length(HH_in_panel)))
# add dates of no purchases to AvgDailyCons dataset
AvgDailyCons <- merge(AvgDailyCons, temp, by=c("Panelist","Date"),all.y=T)
rm(temp)
AvgDailyCons <- AvgDailyCons[with(AvgDailyCons,order(Panelist,Date)),]
row.names(AvgDailyCons) <- seq(1:nrow(AvgDailyCons))
AvgDailyCons$Q <- as.numeric(AvgDailyCons$Q)
AvgDailyCons$Q[is.na(AvgDailyCons$Q)] <- 0

#PER YEAR#
AvgDailyCons$Cbar <- rep(Cbar_year$Cbar, c(rep(c(365,365,366), times=length(HH_in_panel))))

# ROLLING AVERAGE #
# Cbar always calculated based on previous 12 months (365 days)
 # for 2018, this will give NA! Instead, impute all of 2018 with the Cbar of 31 Dec 2018 (first one that works)
#AvgDailyCons <- AvgDailyCons %>% 
#  group_by(Panelist) %>% 
#  mutate(Cbar = rollmean(Q, 365, fill = NA, align = "right")) %>%
#  fill(Cbar, .direction = "up")




######################
# household inventory
######################

HH_Inv <- AvgDailyCons

# standardize everything to 500g packs
HH_Inv$Q <- HH_Inv$Q / 500
HH_Inv$Cbar <- HH_Inv$Cbar / 500 

# turn into list of lists
   # every HH is a list, containing: Date, Q, Cbar, Inv
HH_Inv <- dlply(HH_Inv, .(Panelist), c)

# needed for some starting values based on IPT
avg_IPT_year <- aggregate(IPT~Panelist+Year, pasta_purch_agg_d, mean)
avg_IPT_year_2019 <- subset(avg_IPT_year, Year=="2019", select= c("Panelist","IPT"))
avg_IPT_19 <- aggregate(IPT~Panelist, subset(pasta_purch_agg_d, Year=="2019"), mean)
avg_IPT_1819 <- aggregate(IPT~Panelist, subset(pasta_purch_agg_d, Year=="2018"|Year=="2019"), mean)
# max IPT calculated across 2018-2019, with hard cut at Dec 31,2019
max_IPT_before2020 <- aggregate(IPT_cut~Panelist, subset(pasta_purch_agg_d_cut, Year=="2018"|Year=="2019"), max)


# calculate daily inventory
for (i in seq_along(HH_in_panel)) {
      ## 7 * Cbar -> Ailawadi
  #HH_Inv[[i]]$Inv[1] <- HH_Inv[[i]]$Cbar[1]*7
      ## average IPT 2019
  #HH_Inv[[i]]$Inv[1] <- HH_Inv[[i]]$Cbar[1] * avg_IPT_year_2019$IPT[avg_IPT_year_2019$Panelist==HH_Inv[[i]]$Panelist[1]]
      ## average IPT (2018&2019)
  #HH_Inv[[i]]$Inv[1] <- HH_Inv[[i]]$Cbar[1] * avg_IPT_1819$IPT[avg_IPT_1819$Panelist==HH_Inv[[i]]$Panelist[1]]
      ## max IPT of 2018-2019
  HH_Inv[[i]]$Inv[1] <- HH_Inv[[i]]$Cbar[1] * max_IPT_before2020$IPT_cut[max_IPT_before2020$Panelist==HH_Inv[[i]]$Panelist[1]]
  for (j in 2:1096) {
    HH_Inv[[i]]$Inv[j] <- HH_Inv[[i]]$Inv[j-1] + HH_Inv[[i]]$Q[j-1] - min(HH_Inv[[i]]$Inv[j-1],HH_Inv[[i]]$Cbar[j-1])
  }
}


# plot average inventory across HH + confidence bounds
# ggplot requires dataframe!!
Inv_plot_input <- data.frame(Date=HH_Inv[[1]]$Date)
for (i in seq_along(HH_in_panel)) {
  if (is.null(HH_Inv[[i]]$Inv)) {}
  else  Inv_plot_input <- cbind(Inv_plot_input, HH_Inv[[i]]$Inv)
} 
Inv_plot_input$Mean <- apply(Inv_plot_input[,2:ncol(Inv_plot_input)],1,mean)
Inv_plot_input$Sd <- apply(Inv_plot_input[,2:(ncol(Inv_plot_input)-1)],1,sd)
Inv_plot_input$lower <- Inv_plot_input$Mean - 1.96*Inv_plot_input$Sd
Inv_plot_input$upper <- Inv_plot_input$Mean + 1.96*Inv_plot_input$Sd
colnames(Inv_plot_input) <- make.unique(names(Inv_plot_input))

#ggplot(subset(Inv_plot_input,!is.na(Mean)), aes(x=Date, y=Mean)) + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3) + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + geom_vline(xintercept=as.Date("2020-10-23"), linetype=4) + ylab("Household inventory of pasta (in packs of 500g)")
ggplot(subset(Inv_plot_input,!is.na(Mean)), aes(x=Date, y=Mean)) + geom_line() + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + geom_vline(xintercept=as.Date("2020-10-23"), linetype=4) + ylim(0,6)+ ylab("Household inventory of pasta (in packs of 500g)")
excl_init <- seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by="days")
ggplot(Inv_plot_input[Inv_plot_input$Date %in% excl_init,], aes(x=Date, y=Mean)) + geom_line()  + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + geom_vline(xintercept=as.Date("2020-10-14"), linetype=4) + ylim(0,6) + ylab("Household inventory of pasta (in packs of 500g)")
zoomdate <- seq(as.Date("2020-01-01"), as.Date("2020-06-30"), by="days")
#ggplot(Inv_plot_input[Inv_plot_input$Date %in% zoomdate,], aes(x=Date, y=Mean)) + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3) + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + geom_vline(xintercept=as.Date("2020-10-23"), linetype=4) + ylab("Household inventory of pasta (in packs of 500g)")
ggplot(Inv_plot_input[Inv_plot_input$Date %in% zoomdate,], aes(x=Date, y=Mean)) + geom_line()  + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + ylim(0,6) + ylab("Household inventory of pasta (in packs of 500g)")
zoomdate2 <- seq(as.Date("2020-07-01"), as.Date("2020-12-31"), by="days")
#ggplot(Inv_plot_input[Inv_plot_input$Date %in% zoomdate2,], aes(x=Date, y=Mean)) + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3) + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + ylab("Household inventory of pasta (in packs of 500g)")
ggplot(Inv_plot_input[Inv_plot_input$Date %in% zoomdate2,], aes(x=Date, y=Mean)) + geom_line()  + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + ylim(0,6) + ylab("Household inventory of pasta (in packs of 500g)")



### plotting inventory of 4 random households
 # + calculating % of days where inventory = 0 

days20192020 <- seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by="days")
par(mfrow=c(2,2))
tempHH <- sample(seq_along(HH_in_panel),4)
for (i in 1:4) {
  plot(HH_Inv[[tempHH[i]]]$Date, HH_Inv[[tempHH[i]]]$Inv, type="l", xlab="Date", ylab="Household inventory of pasta (in packs of 500g)", 
       main=paste("days of zero inventory in 2019-2020: ",sum(HH_Inv[[tempHH[[i]]]]$Date[HH_Inv[[tempHH[[i]]]]$Inv==0] %in% days20192020), sep=""))
}

# for all households in panel: calculating number of days in 2019-2020 where inventory = 0 
temp <- data.frame(Panelist = HH_in_panel, ZeroInv = NA)
for (i in seq_along(HH_in_panel)) {
  temp$ZeroInv[i] <- sum(HH_Inv[[as.character(HH_in_panel[i])]]$Date[HH_Inv[[as.character(HH_in_panel[i])]]$Inv==0] %in% days20192020)
}
summary(temp$ZeroInv)
ggplot(temp, aes(x=ZeroInv)) + geom_density() + xlab("Days of zero inventory in 2019-2020")
ggplot(temp, aes(x=ZeroInv)) + geom_histogram() + xlab("Days of zero inventory in 2019-2020")




###########################
# find hoarding households
###########################

# 10-03 until 09-04
hoardHH <- data.frame(HH_ID = names(HH_Inv))
for (i in seq_along(HH_in_panel)) {
  hoardHH$flag[i] <- ifelse(mean(HH_Inv[[i]]$Inv[800:830]) > 1*sd(HH_Inv[[i]]$Inv) + mean(HH_Inv[[i]]$Inv[771:799]),1,0)
}
table(hoardHH$flag)
prop.table(table(hoardHH$flag))

# 1-03 until 31-03
hoardHH <- data.frame(HH_ID = names(HH_Inv))
for (i in seq_along(HH_in_panel)) {
  hoardHH$flag[i] <- ifelse(mean(HH_Inv[[i]]$Inv[791:821]) > 1*sd(HH_Inv[[i]]$Inv) + mean(HH_Inv[[i]]$Inv[760:790]),1,0)
}
table(hoardHH$flag)
prop.table(table(hoardHH$flag))


# 4-03 until 29-03
hoardHH <- data.frame(HH_ID = names(HH_Inv))
for (i in seq_along(HH_in_panel)) {
  hoardHH$flag[i] <- ifelse(mean(HH_Inv[[i]]$Inv[794:819]) > 1*sd(HH_Inv[[i]]$Inv) + mean(HH_Inv[[i]]$Inv[768:793]),1,0)
}
table(hoardHH$flag)
prop.table(table(hoardHH$flag))
ggplot(Inv_plot_input[Inv_plot_input$Date %in% zoomdate,], aes(x=Date, y=Mean)) + geom_line()  + geom_vline(xintercept=as.Date("2020-03-04"), linetype=4)  + geom_vline(xintercept=as.Date("2020-03-29"), linetype=4) + ylim(0,6) + ylab("Household inventory of pasta (in packs of 500g)")






################################################################
############## FLEXIBLE CONSUMPTION MODEL ######################


# Inventory on 1-1-2019 = 7 * Cbar calculated over 2018 (initialization period)

# Inv = Inv_t-1 + Q - CONS
# CONS = Inv[Cbar / (Cbar + Inv^f)]

#   ==>    Inv = Inv_t-1 + Q_t-1 - Inv_t-1(Cbar / (Cbar + Inv_t-1^f))

# calculate for each household Cbar 
   # = average daily consumption level during initialization period 2018
Cbar <- pasta_purch_agg_y$tot_grams[pasta_purch_agg_y$Year == "2018"] / 365

include_2018 <- "yes"
# create dataframe with all inputs, starting on 1-1-2019 or 1-1-2018
if (include_2018 == "no") { 
  flexcons <- pasta_purch_agg_d[pasta_purch_agg_d$Year!="2018",c("Panelist","Date_of_purchase","tot_grams")]
  names(flexcons) <- c("Panelist","Date","Q")
  # create dataframe of all dates-HH combos
  HH_in_panel <- unique(pasta_purch$Panelist)
  all.dates <- seq(as.Date("2019-01-01"),as.Date("2020-12-31"),by="days")
  temp <- data.frame(Panelist = rep(HH_in_panel, length(all.dates)), Date= rep(all.dates, each=length(HH_in_panel)))
  # add dates of no purchases to flexcons dataset
  flexcons <- merge(flexcons, temp, by=c("Panelist","Date"),all.y=T)
  rm(temp)
  flexcons <- flexcons[with(flexcons,order(Panelist,Date)),]
  row.names(flexcons) <- seq(1:nrow(flexcons))
  flexcons$Q <- as.numeric(flexcons$Q)
  flexcons$Q[is.na(flexcons$Q)] <- 0
  flexcons$Cbar <- rep(Cbar, each= length(all.dates))
} else if (include_2018 == "yes") {
  flexcons <- pasta_purch_agg_d[,c("Panelist","Date_of_purchase","tot_grams")]
  names(flexcons) <- c("Panelist","Date","Q")
  # create dataframe of all dates-HH combos
  HH_in_panel <- unique(pasta_purch$Panelist)
  all.dates <- seq(as.Date("2018-01-01"),as.Date("2020-12-31"),by="days")
  temp <- data.frame(Panelist = rep(HH_in_panel, length(all.dates)), Date= rep(all.dates, each=length(HH_in_panel)))
  # add dates of no purchases to flexcons dataset
  flexcons <- merge(flexcons, temp, by=c("Panelist","Date"),all.y=T)
  rm(temp)
  flexcons <- flexcons[with(flexcons,order(Panelist,Date)),]
  row.names(flexcons) <- seq(1:nrow(flexcons))
  flexcons$Q <- as.numeric(flexcons$Q)
  flexcons$Q[is.na(flexcons$Q)] <- 0
  flexcons$Cbar <- rep(Cbar, each= length(all.dates))
  }

# flexible consumption parameter
f = 1


HH_Inv <- flexcons
# standardize everything to 500g packs
HH_Inv$Q <- HH_Inv$Q / 500
HH_Inv$Cbar <- HH_Inv$Cbar / 500 
# turn into list of lists
# every HH is a list, containing: Date, Q, Cbar, Inv
HH_Inv <- dlply(HH_Inv, .(Panelist), c)

max_IPT_before2020 <- aggregate(IPT_cut~Panelist, subset(pasta_purch_agg_d_cut, Year=="2018"|Year=="2019"), max)

if (include_2018 == "no") {
  for (i in seq_along(HH_in_panel)) {
    ## starting inventory = 7 * Cbar -> Ailawadi
    HH_Inv[[i]]$Inv[1] <- HH_Inv[[i]]$Cbar[1]*7
    ## starting inventory = max IPT of 2018-2019 with hard cut
    #HH_Inv[[i]]$Inv[1] <- HH_Inv[[i]]$Cbar[1] * max_IPT_before2020$IPT_cut[max_IPT_before2020$Panelist==HH_Inv[[i]]$Panelist[1]]
    for (j in 2:731) {
      HH_Inv[[i]]$Inv[j] <- HH_Inv[[i]]$Inv[j-1] + HH_Inv[[i]]$Q[j-1] - HH_Inv[[i]]$Inv[j-1]*( HH_Inv[[i]]$Cbar[j-1]/ (HH_Inv[[i]]$Cbar[j-1]+ (HH_Inv[[i]]$Inv[j-1])^f) )
  }}
} else if (include_2018 == "yes") {
  for (i in seq_along(HH_in_panel)) {
    ## starting inventory = 7 * Cbar -> Ailawadi
    HH_Inv[[i]]$Inv[1] <- HH_Inv[[i]]$Cbar[1]*7
    ## starting inventory = max IPT of 2018-2019 with hard cut
    #HH_Inv[[i]]$Inv[1] <- HH_Inv[[i]]$Cbar[1] * max_IPT_before2020$IPT_cut[max_IPT_before2020$Panelist==HH_Inv[[i]]$Panelist[1]]
    for (j in 2:1096) {
      HH_Inv[[i]]$Inv[j] <- HH_Inv[[i]]$Inv[j-1] + HH_Inv[[i]]$Q[j-1] - HH_Inv[[i]]$Inv[j-1]*( HH_Inv[[i]]$Cbar[j-1]/ (HH_Inv[[i]]$Cbar[j-1]+ (HH_Inv[[i]]$Inv[j-1])^f) )
    }}
}


# plot average inventory across HH + confidence bounds
# ggplot requires dataframe!!
Inv_plot_input <- data.frame(Date=HH_Inv[[1]]$Date)
for (i in seq_along(HH_in_panel)) {
  if (is.null(HH_Inv[[i]]$Inv)) {}
  else  Inv_plot_input <- cbind(Inv_plot_input, HH_Inv[[i]]$Inv)
} 
Inv_plot_input$Mean <- apply(Inv_plot_input[,2:ncol(Inv_plot_input)],1,mean)
Inv_plot_input$Sd <- apply(Inv_plot_input[,2:(ncol(Inv_plot_input)-1)],1,sd)
Inv_plot_input$lower <- Inv_plot_input$Mean - 1.96*Inv_plot_input$Sd
Inv_plot_input$upper <- Inv_plot_input$Mean + 1.96*Inv_plot_input$Sd
colnames(Inv_plot_input) <- make.unique(names(Inv_plot_input))

# plot average inventory across households
ggplot(subset(Inv_plot_input,!is.na(Mean)), aes(x=Date, y=Mean)) + ylim(0,2000)+ geom_line() + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + geom_vline(xintercept=as.Date("2020-10-23"), linetype=4)+ ylab("Household inventory of pasta (in packs of 500g)")
zoomdate <- seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by="days")
ggplot(Inv_plot_input[Inv_plot_input$Date %in% zoomdate,], aes(x=Date, y=Mean)) + ylim(0,2) + geom_line()  + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + geom_vline(xintercept=as.Date("2020-10-23"), linetype=4) + ylab("Household inventory of pasta (in packs of 500g)")

#plot inventory for 4 random households 
days20192020 <- seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by="days")
par(mfrow=c(2,2))
tempHH <- sample(seq_along(HH_in_panel),4)
for (i in 1:4) {
  plot(HH_Inv[[tempHH[i]]]$Date, HH_Inv[[tempHH[i]]]$Inv, type="l", xlab="Date", ylab="Household inventory of pasta (in packs of 500g)", 
       main=paste("days of zero inventory in 2019-2020: ",sum(HH_Inv[[tempHH[[i]]]]$Date[HH_Inv[[tempHH[[i]]]]$Inv==0] %in% days20192020), sep=""))
}
for (i in 1:4) {
  plot(HH_Inv[[tempHH[i]]]$Date[HH_Inv[[tempHH[i]]]$Date %in% zoomdate], HH_Inv[[tempHH[i]]]$Inv[HH_Inv[[tempHH[i]]]$Date %in% zoomdate], type="l", xlab="Date", ylab="Household inventory of pasta (in packs of 500g)", 
       main=paste("days of zero inventory in 2019-2020: ",sum(HH_Inv[[tempHH[[i]]]]$Date[HH_Inv[[tempHH[[i]]]]$Inv==0] %in% days20192020), sep=""))
}
