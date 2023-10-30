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


# if working with server: better to prepare toilet paper dataset once and saving it as RData file -> faster to load from Xdrive
#load(file=paste("./R_Files/Preparation/toilet paper/prepared_toiletpaper_",country,".RData",sep=""))


# otherwise:
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

if (country =="UK") {
  colnames(toiletpaper)[1] <- "Barcode"
  colnames(toiletpaper)[2] <- "Barcode_description"
  }
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

if (country == "GE") {
  toiletpaper$Product <- factor(toiletpaper$Product)
  toiletpaper$Manufacturer <- factor(toiletpaper$Manufacturer)
  toiletpaper$brand_as_delivered <- factor(toiletpaper$brand_as_delivered)
  toiletpaper$MFP <- factor(toiletpaper$MFP)
} else if (country == "BE") {
  toiletpaper$Volume_per_unit <- factor(toiletpaper$Volume_per_unit)
} else if (country == "UK") {
  toiletpaper$Number_in_a_Pack <- factor(toiletpaper$Number_in_a_Pack)
}


####################################
# Creating sheet & ply variables
####################################
toiletpaper$ply <- NA
toiletpaper$sheet <- NA

if (country == "NL") {

  toiletpaper$ply <- as.integer(sub(".*?(\\d+)\\s*-laags.*", "\\1", x=toiletpaper$Barcode_description))
  toiletpaper[is.na(toiletpaper$ply),]      # one observation where description is NA -> no ply info

  # toiletpaper$sheet <- as.integer(sub(".*?(\\d+)\\s* vel.*", "\\1", x=toiletpaper$Barcode_description))
  # toiletpaper[is.na(toiletpaper$sheet),]   # one observation where description is NA -> no sheet info

} else if (country == "GE") {
  toiletpaper$ply <- as.integer(gsub(".*?(\\d+)\\s* L.*", "\\1", x=toiletpaper$Barcode_description))
  toiletpaper$ply[is.na(toiletpaper$ply)] <- as.integer(sub(".*?(\\d+)\\s*-L.*", "\\1", x=toiletpaper$Barcode_description[is.na(toiletpaper$ply)]))
  toiletpaper$ply[is.na(toiletpaper$ply)] <- as.integer(sub(".*?(\\d+)\\s*L.*", "\\1", x=toiletpaper$Barcode_description[is.na(toiletpaper$ply)]))
  toiletpaper$ply[is.na(toiletpaper$ply)] <- as.integer(sub(".*?(\\d+)\\s* l", "\\1", x=toiletpaper$Barcode_description[is.na(toiletpaper$ply)]))
  toiletpaper$ply[is.na(toiletpaper$ply)] <- as.integer(sub(".*?(\\d+)\\RL s*", "\\1", x=toiletpaper$Barcode_description[is.na(toiletpaper$ply)]))

  # toiletpaper$sheet <- as.integer(sub(".*?(\\d+)\\s*BL", "\\1", x=toiletpaper$Barcode_description))
  # toiletpaper$sheet[is.na(toiletpaper$sheet)] <- as.integer(sub(".*?(\\d+)\\s*BLATT.*", "\\1", x=toiletpaper$Barcode_description[is.na(toiletpaper$sheet)]))
  # toiletpaper$sheet[is.na(toiletpaper$sheet)] <- as.integer(sub(".*?(\\d+)\\s*BLATT", "\\1", x=toiletpaper$Barcode_description[is.na(toiletpaper$sheet)]))
  # toiletpaper$sheet[is.na(toiletpaper$sheet)] <- as.integer(sub(".*?(\\d+)\\s*BL.*", "\\1", x=toiletpaper$Barcode_description[is.na(toiletpaper$sheet)]))
  # toiletpaper$sheet[is.na(toiletpaper$sheet)] <- as.integer(sub(".*?(\\d+)\\s*B.*", "\\1", x=toiletpaper$Barcode_description[is.na(toiletpaper$sheet)]))
  # toiletpaper$sheet[toiletpaper$Barcode == "7322540504910"] <- 150
} else if (country == "BE") {
  toiletpaper$ply <- as.integer(sub(".*?(\\d+)\\s*-laags.*", "\\1", x=toiletpaper$Barcode_description))
  toiletpaper[is.na(toiletpaper$ply),]      # one observation where description is NA -> no ply info

} else if (country == "FR") {
} else if (country == "UK") {
  toiletpaper$ply <- as.integer(sub(".*?\\/(\\d+).*","\\1", x=toiletpaper$Barcode_description))
  toiletpaper$ply[toiletpaper$ply=="21"] <- NA
  toiletpaper$sheet <- as.integer(sub(".*?(\\d+)\\/.*","\\1", x=toiletpaper$Barcode_description))
  }

if (country=="UK") {
  toiletpaper <- toiletpaper[,c("Barcode","Barcode_description","PL","Brand","Original_Brand",
                                "Sub_brand","New_Sub_Brand","Category_name","Sub_category_name",
                                "Measurement_unit","Number_in_a_Pack","BG_Category_number",
                                "BG_Category_name","ply","sheet")]
}



#######################
# load purchase data 
#######################


if (location == "server") {
  #load purchase dataset of filtered HHs
  load(paste(save_path,"/filteredPurchdata_", country,".RData", sep=""))
  purch$Barcode <- factor(purch$Barcode)
  TP_purch <- merge(purch, toiletpaper, by="Barcode")
  rm(purch)
} else if (location == "local") {
  if (country == "NL") {
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
  } else if (country == "UK") {
    purch2018 <- read.csv(paste(data_path,"/",country,"/purchase_2018.csv",sep=""))
    colnames(purch2018)[colnames(purch2018)=="Product_code"] <- "Barcode"
    purch2018$Barcode <- factor(purch2018$Barcode)
    TP_purch <- merge(purch2018,toiletpaper,by="Barcode")
    rm(purch2018)
    purch2019 <- read.csv(paste(data_path,"/",country,"/purchase_2019.csv",sep=""))
    colnames(purch2019)[colnames(purch2019)=="Product_code"] <- "Barcode"
    purch2019$Barcode <- factor(purch2019$Barcode)
    TP_purch <- rbind(TP_purch,merge(purch2019,toiletpaper,by="Barcode"))
    rm(purch2019)
    purch2020 <- read.csv(paste(data_path,"/",country,"/purchase_2020.csv",sep=""))
    colnames(purch2020)[colnames(purch2020)=="Product_code"] <- "Barcode"
    purch2020$Barcode <- factor(purch2020$Barcode)
    TP_purch <- rbind(TP_purch,merge(purch2020,toiletpaper,by="Barcode"))
    rm(purch2020)
  } else {
    purch2018 <- read.csv(paste(data_path,"/",country,"/purchase_2018.csv",sep=""))
    purch2019 <- read.csv(paste(data_path,"/",country,"/purchase_2019.csv",sep=""))
    purch2020 <- read.csv(paste(data_path,"/",country,"/purchase_2020.csv",sep=""))
    purch2018$Barcode <- factor(purch2018$Barcode)
    purch2019$Barcode <- factor(purch2019$Barcode)
    purch2020$Barcode <- factor(purch2020$Barcode)
    #merge purchase data with toilet paper barcode
    TP_purch <- merge(purch2018,toiletpaper,by="Barcode")
    TP_purch <- rbind(TP_purch,merge(purch2019,toiletpaper,by="Barcode"))
    TP_purch <- rbind(TP_purch, merge(purch2020,toiletpaper,by="Barcode"))
    rm(list=c('purch2018','purch2019','purch2020'))
  }
 # only keep purchase data of households that passed the general filter
       # "each household should buy 'something' at least once every quarter"
  load(file= paste(project_path,"/R_Files/Preparation/household filters/", country,"filtered_HH_ID.RData", sep=""))
  TP_purch <- TP_purch[TP_purch$Panelist %in% panelists_keep_ID,]
}





# ###############################
# # Create sheets*rolls variable
# ###############################
# 
# TP_purch$ply <- factor(TP_purch$ply)
# 
# TP_purch$tot_sheet <- TP_purch$sheet * TP_purch$Volume_per_unit * TP_purch$Total_unit_sales
# 
# # create sheets*rolls variable per ply type
# for (i in levels(TP_purch$ply)) { 
#   interim <- ifelse(TP_purch$ply==i,TP_purch$Volume_per_unit*TP_purch$sheet*TP_purch$Total_unit_sales,0)
#   #interim <- ifelse(TP_purch$ply==i,TP_purch$Volume_per_unit[TP_purch$ply==i]*TP_purch$sheet[TP_purch$ply==i],0)
#   TP_purch <- cbind(TP_purch, interim)
#   names(TP_purch)[names(TP_purch) == "interim"] <- paste("tot_sheet_ply",i, sep="")
#   rm('interim')
# }
# 
# # create sheets*rolls variable for PL vs. no PL
# #TP_purch$tot_sheet_PL <- ifelse(TP_purch$PL==T,TP_purch$Volume_per_unit[TP_purch$PL==T]*TP_purch$sheet[TP_purch$PL==T],0)
# #TP_purch$tot_sheet_noPL <- ifelse(TP_purch$PL==F,TP_purch$Volume_per_unit[TP_purch$PL==F]*TP_purch$sheet[TP_purch$PL==F],0)
# TP_purch$tot_sheet_PL <- ifelse(TP_purch$PL==T,TP_purch$Volume_per_unit*TP_purch$sheet*TP_purch$Total_unit_sales,0)
# TP_purch$tot_sheet_noPL <- ifelse(TP_purch$PL==F,TP_purch$Volume_per_unit*TP_purch$sheet*TP_purch$Total_unit_sales,0)





#########################################
# inspecting toilet paper purchase data
#########################################
str(TP_purch)
TP_purch$Date_of_purchase <- as.Date(TP_purch$Date_of_purchase)
if (country == "UK") {TP_purch$Shopcode <- factor(TP_purch$Shopcode)
} else { TP_purch$Banner_name <- factor(TP_purch$Banner_name) } 
TP_purch$Panelist <- factor(TP_purch$Panelist)

if (country== "NL") {
TP_purch$Promo <- ifelse(TP_purch$Promo=="yes",1,0)
TP_purch$Promo <- as.logical(TP_purch$Promo) } 

if (country=="GE") { colnames(TP_purch)[8] <- "Quarter" }
TP_purch$Quarter <- factor(TP_purch$Quarter)
TP_purch$Year <- as.numeric(substr(TP_purch$Quarter,1,4))
TP_purch$Year <- factor(TP_purch$Year)

TP_purch$HalfYear <- paste(TP_purch$Year, ifelse(substr(TP_purch$Quarter,5,6)=="01"|substr(TP_purch$Quarter,5,6)=="02","H1",""),
                           ifelse(substr(TP_purch$Quarter,5,6)=="03"|substr(TP_purch$Quarter,5,6)=="04","H2",""),sep="")

#international fixed calendar
load(paste(project_path,"/R_Files/Preparation/conversion_IFC.RData", sep=""))
TP_purch <- merge(TP_purch, IFC, by.x="Date_of_purchase",by.y="Date")

if (country == "UK") { TP_purch$Total_volume_sales <- as.integer(sub("(\\d+)\\s* in a Pack.*","\\1", x=TP_purch$Number_in_a_Pack))
                       TP_purch$Total_volume_sales[is.na(TP_purch$Total_volume_sales) & TP_purch$Number_in_a_Pack=="Single"] <- 1
                       TP_purch$Total_volume_sales[is.na(TP_purch$Total_volume_sales) & TP_purch$Number_in_a_Pack=="10 X 4 In a Pack"] <- 40
                       TP_purch$Total_volume_sales[is.na(TP_purch$Total_volume_sales) & TP_purch$Number_in_a_Pack=="2 X 9 In a Pack"] <- 18
                       TP_purch$Total_volume_sales[is.na(TP_purch$Total_volume_sales) & TP_purch$Number_in_a_Pack=="2 X 9 in a Pack"] <- 18
                       TP_purch$Total_volume_sales[is.na(TP_purch$Total_volume_sales) & TP_purch$Number_in_a_Pack=="5 X 9 In a Pack"] <- 45  }

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



### COMPARING CHARACTERISTICS OF KEPT VS. DROPPED HOUSEHOLDS ###

# loading HH data to get characteristics
if (location == "server") {
  load(paste(save_path,"/filteredHHdata_",country,".RData", sep=""))
} else if (location == "local") {
  HH <- read.csv(paste(data_path, "/", country, "/panelist.csv", sep=""))
}
# only keep HH info of panelists in TP dataset
HH <- HH[HH$Panelist %in% unique(TP_purch$Panelist),]

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

# TP_purch_agg_y <- ddply(TP_purch, .(Panelist,Year), summarise, tot_sheet = sum(tot_sheet),
#                         tot_sheet_PL=sum(tot_sheet_PL),tot_sheet_noPL=sum(tot_sheet_noPL), 
#                         tot_sheet_ply1 = sum(tot_sheet_ply1),tot_sheet_ply2 = sum(tot_sheet_ply2),
#                         tot_sheet_ply3 = sum(tot_sheet_ply3),tot_sheet_ply4 = sum(tot_sheet_ply4),
#                         tot_sheet_ply5 = sum(tot_sheet_ply5))

TP_purch_agg_y <- ddply(TP_purch, .(Panelist,Year), summarise, tot_rolls = sum(Total_volume_sales))






#####################
# model-free evidence
#####################

### AVERAGE PRICES ###
TP_purch$AvgPrice_pack <- TP_purch$Total_value_sales / TP_purch$Total_unit_sales * 0.01
TP_purch$AvgPrice_roll <- TP_purch$Total_value_sales / TP_purch$Total_volume_sales * 0.01
#TP_purch$AvgPrice_200sheet <- TP_purch$AvgPrice_roll / TP_purch$sheet * 200
# average price of pack, per ply type (packs can contain different number of rolls, of different size!)
ddply(TP_purch, "ply", summarise, avg_price_pack=mean(AvgPrice_pack))
# average price of roll, per ply type (rolls can be of different size!)
ddply(TP_purch, "ply", summarise, avg_price_roll = mean(AvgPrice_roll))
# average price of 200 sheet roll, per ply type
#ddply(TP_purch, "ply", summarise, avg_price_200sheet = mean(AvgPrice_200sheet))

### PLY ###
# SKUs per ply type
ggplot(toiletpaper, aes(x=factor(ply))) + geom_bar(position="dodge", show.legend=F)
# calculating market shares of "ply type" per year
ply_MS <- ddply(TP_purch, .(Year, ply), summarise, Total_unit_sales = sum(Total_unit_sales),
                Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
year_sum <- ddply(TP_purch, .(Year), summarise, Total_unit_sales = sum(Total_unit_sales),
                 Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
year_sum <- (do.call("rbind", replicate(length(unique(TP_purch$ply)), year_sum, simplify = FALSE)))
year_sum <- year_sum[order(year_sum$Year),]
ply_MS[,3:5] <- ply_MS[,3:5] / year_sum[,2:4]
ply_MS[,3:5] <- round(ply_MS[,3:5],4)
# total unit market shares
plot_ply_MS_unit <- ggplot(ply_MS, aes(x=Year,y=Total_unit_sales,fill=factor(ply))) + geom_bar(position="dodge", stat="identity")
ply_MS[order(ply_MS$ply),c("ply","Year","Total_unit_sales")]
# total volume market shares
plot_ply_MS_volume <- ggplot(ply_MS, aes(x=Year,y=Total_volume_sales,fill=factor(ply))) + geom_bar(position="dodge", stat="identity")
ply_MS[order(ply_MS$ply),c("ply","Year","Total_volume_sales")]
# total value market shares
plot_ply_MS_value <- ggplot(ply_MS, aes(x=Year,y=Total_value_sales,fill=factor(ply))) + geom_bar(position="dodge", stat="identity")
ply_MS[order(ply_MS$ply),c("ply","Year","Total_value_sales")]

 
# ### SHEETS ###
# # SKUs per sheet type
# ggplot(toiletpaper, aes(x=factor(sheet))) + geom_bar(position="dodge", show.legend=F)
# # calculating market shares of "sheet type" per year
# sheet_MS <- ddply(TP_purch, .(Year, sheet), summarise, Total_unit_sales = sum(Total_unit_sales),
#                Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
# year_sum <- ddply(TP_purch, .(Year), summarise, Total_unit_sales = sum(Total_unit_sales),
#                  Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
# for (i in seq_along(sheet_MS$sheet)) {
#  sheet_MS[i,3:5] <- sheet_MS[i,3:5] / year_sum[year_sum$Year==sheet_MS$Year[i], 2:4]
# }
# sheet_MS[,3:5] <- round(sheet_MS[,3:5],4)
# # total unit market shares
# plot_sheet_MS_unit <- ggplot(sheet_MS, aes(x=Year,y=Total_unit_sales,fill=factor(sheet))) + geom_bar(position="dodge", stat="identity")
# sheet_MS[order(sheet_MS$sheet),c("sheet","Year","Total_unit_sales")]
# # total volume market shares
# plot_sheet_MS_volume <- ggplot(sheet_MS, aes(x=Year,y=Total_volume_sales,fill=factor(sheet))) + geom_bar(position="dodge", stat="identity")
# sheet_MS[order(sheet_MS$sheet),c("sheet","Year","Total_volume_sales")]
# # total value market shares
# plot_sheet_MS_value <- ggplot(sheet_MS, aes(x=Year,y=Total_value_sales,fill=factor(sheet))) + geom_bar(position="dodge", stat="identity")
# sheet_MS[order(sheet_MS$sheet),c("sheet","Year","Total_value_sales")]

 
### PRIVATE LABEL ###
ggplot(toiletpaper, aes(x=factor(PL))) + geom_bar(position="dodge", show.legend=F)
# calculating market shares of "PL type" per year
PL_MS <- ddply(TP_purch, .(Year, PL), summarise, Total_unit_sales = sum(Total_unit_sales),
               Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
year_sum <- ddply(TP_purch, .(Year), summarise, Total_unit_sales = sum(Total_unit_sales),
                 Total_volume_sales = sum(Total_volume_sales), Total_value_sales = sum(Total_value_sales))
year_sum <- (do.call("rbind", replicate(length(unique(TP_purch$PL)), year_sum, simplify = FALSE)))
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
 
 
 
### TOTAL QUANTITY/VALUE/VOLUME SOLD + AVERAGE PRICE ###
# PER DAY
tot_Q <- ddply(TP_purch, .(Date_of_purchase, Year), summarize, Tot_unit_sales = sum(Total_unit_sales),
              Tot_volume_sales=sum(Total_volume_sales), Tot_value_sales=sum(Total_value_sales))
tot_Q$avgP <- tot_Q$Tot_value_sales / tot_Q$Tot_volume_sales
tot_Q$Date_of_purchase <- sub(pattern="2018-","",x=tot_Q$Date_of_purchase)
tot_Q$Date_of_purchase <- sub(pattern="2019-","",x=tot_Q$Date_of_purchase)
tot_Q$Date_of_purchase <- sub(pattern="2020-","",x=tot_Q$Date_of_purchase)
ggplot(tot_Q, aes(x=Date_of_purchase, y=Tot_unit_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Date_of_purchase, y=Tot_volume_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Date_of_purchase, y=Tot_value_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Date_of_purchase, y=avgP, group=Year)) + geom_line(aes(colour=factor(Year))) + labs(y="average price per roll in cents") +
#  geom_vline(xintercept=sub(pattern="2020-", "",x=as.Date("2020-03-18")),linetype=4) + 
#  geom_vline(xintercept = sub(pattern="2020-","", x=as.Date("2020-05-01")), linetype=4)
rm(tot_Q)

# PER IFC WEEK
tot_Q <- ddply(TP_purch[!is.na(TP_purch$Week_IFC),], .(Week_IFC, Year), summarize, Tot_unit_sales = sum(Total_unit_sales),
              Tot_volume_sales=sum(Total_volume_sales), Tot_value_sales=sum(Total_value_sales))
tot_Q$avgP <- tot_Q$Tot_value_sales / tot_Q$Tot_volume_sales
ggplot(tot_Q, aes(x=Week_IFC, y=Tot_unit_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Week_IFC, y=Tot_volume_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Week_IFC, y=Tot_value_sales, group=Year)) + geom_line(aes(colour=factor(Year)))
ggplot(tot_Q, aes(x=Week_IFC, y=avgP, group=Year)) + geom_line(aes(colour=factor(Year))) + labs(y="average price per roll in cents") +
#  geom_vline(xintercept = "12", linetype=4) +
#  geom_vline(xintercept = "13", linetype=4) +
#  geom_vline(xintercept = "18", linetype=4)
rm(tot_Q)

 
 
### SALES VOLUME PER HOUSEHOLD OVER TIME ###
# PER DAY
Q_per_HH <- ddply(TP_purch, .(Date_of_purchase, Year), summarize, Tot_volume_sales=sum(Total_volume_sales), HH_buying = length(Date_of_purchase))
Q_per_HH$Q_HH <- Q_per_HH$Tot_volume_sales / Q_per_HH$HH_buying
Q_per_HH$Date_of_purchase <- sub(pattern="2018-","",x=Q_per_HH$Date_of_purchase)
Q_per_HH$Date_of_purchase <- sub(pattern="2019-","",x=Q_per_HH$Date_of_purchase)
Q_per_HH$Date_of_purchase <- sub(pattern="2020-","",x=Q_per_HH$Date_of_purchase)
ggplot(Q_per_HH, aes(x=Date_of_purchase, y=Q_HH, group=Year)) + geom_line(aes(colour=factor(Year))) + labs(y="average Q per Household")
rm (Q_per_HH)

# PER IFC WEEK
Q_per_HH <- ddply(TP_purch[!is.na(TP_purch$Week_IFC),], .(Week_IFC, Year), summarize, Tot_volume_sales=sum(Total_volume_sales), HH_buying = length(Week_IFC))
Q_per_HH$Q_HH <- Q_per_HH$Tot_volume_sales / Q_per_HH$HH_buying
ggplot(Q_per_HH, aes(x=Week_IFC, y=Q_HH, group=Year)) + geom_line(aes(colour=factor(Year))) + labs(y="average Q per Household") + ylim(0,13)
rm (Q_per_HH)



### AVERAGE TIME SINCE PREVIOUS PURCHASE ###
PrevPurch <- aggregate(IPT ~ Date_of_purchase, TP_purch_agg_d, mean)
ggplot(PrevPurch, aes(x=Date_of_purchase, y=IPT)) + geom_line() + 
  geom_vline(xintercept=as.Date("2020-03-16"), col="blue") + 
  geom_vline(xintercept=as.Date("2020-03-26"), col="blue") + 
  geom_vline(xintercept=as.Date("2020-11-05"), col="blue") + 
  geom_hline(yintercept = mean(PrevPurch$IPT), col= "red")

rm(PrevPurch)




##########################################
# average daily consumption per household
##########################################

# PER YEAR #
avg_daily_cons <- data.frame(HH_ID = unique(TP_purch_agg_d$Panelist))
days_2018 <- 365
days_2019 <- 365
days_2020 <- 366

# for (i in seq_along(avg_daily_cons$HH_ID)) {
#  avg_daily_cons$Y2018[i] <- TP_purch_agg_y$tot_sheet[TP_purch_agg_y$Panelist==avg_daily_cons$HH_ID[i] & TP_purch_agg_y$Year==2018] / days_2018
#  avg_daily_cons$Y2019[i] <- TP_purch_agg_y$tot_sheet[TP_purch_agg_y$Panelist==avg_daily_cons$HH_ID[i] & TP_purch_agg_y$Year==2019] / days_2019
#  avg_daily_cons$Y2020[i] <- TP_purch_agg_y$tot_sheet[TP_purch_agg_y$Panelist==avg_daily_cons$HH_ID[i] & TP_purch_agg_y$Year==2020] / days_2020
# }
for (i in seq_along(avg_daily_cons$HH_ID)) {
  avg_daily_cons$Y2018[i] <- TP_purch_agg_y$tot_rolls[TP_purch_agg_y$Panelist==avg_daily_cons$HH_ID[i] & TP_purch_agg_y$Year==2018] / days_2018
  avg_daily_cons$Y2019[i] <- TP_purch_agg_y$tot_rolls[TP_purch_agg_y$Panelist==avg_daily_cons$HH_ID[i] & TP_purch_agg_y$Year==2019] / days_2019
  avg_daily_cons$Y2020[i] <- TP_purch_agg_y$tot_rolls[TP_purch_agg_y$Panelist==avg_daily_cons$HH_ID[i] & TP_purch_agg_y$Year==2020] / days_2020
}

# PER QUARTER #
 # not a good idea!
 # if calculated per quarter, the average daily consumption is driven more by purchase than consumption



# ROLLING AVERAGE #
# create dataframe with all dates (not only ones with purchases)
# AvgDailyCons <- TP_purch_agg_d[,c("Panelist","Date_of_purchase","tot_sheet")]
AvgDailyCons <- TP_purch_agg_d[,c("Panelist","Date_of_purchase","tot_rolls")]
names(AvgDailyCons) <- c("Panelist","Date","Q")
# create dataframe of all dates-HH combos
HH_in_panel <- unique(TP_purch$Panelist)
all.dates <- seq(as.Date("2018-01-01"),as.Date("2020-12-31"),by="days")
temp <- data.frame(Panelist = rep(HH_in_panel, length(all.dates)), Date= rep(all.dates, each=length(HH_in_panel)))
# add dates of no purchases to AvgDailyCons dataset
AvgDailyCons <- merge(AvgDailyCons, temp, by=c("Panelist","Date"),all.y=T)
rm(temp)
AvgDailyCons <- AvgDailyCons[with(AvgDailyCons,order(Panelist,Date)),]
row.names(AvgDailyCons) <- seq(1:nrow(AvgDailyCons))
AvgDailyCons$Q <- as.numeric(AvgDailyCons$Q)
AvgDailyCons$Q[is.na(AvgDailyCons$Q)] <- 0

# Cbar always calculated based on previous 12 months (365 days)
# for 2018, this will give NA! Instead, impute all of 2018 with the Cbar of 31 Dec 2018 (first one that works)
AvgDailyCons <- AvgDailyCons %>%
  group_by(Panelist) %>%
  mutate(Cbar = rollmean(Q, 365, fill = NA, align = "right")) %>%
  fill(Cbar, .direction = "up")





#################################
# calculating household inventory
#################################

HH_Inv <- AvgDailyCons

# standardize everything to 200-sheet rolls
# HH_Inv$Q <- HH_Inv$Q / 200
# HH_Inv$Cbar <- HH_Inv$Cbar / 200

# turn into list of lists
  # every HH is a list, containing: Date, Q, Cbar, Inv
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

#ggplot(subset(Inv_plot_input,!is.na(Mean)), aes(x=Date, y=Mean)) + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3) + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + ylab("Household inventory of toilet paper (in rolls)")
ggplot(subset(Inv_plot_input,!is.na(Mean)), aes(x=Date, y=Mean)) + geom_line() + 
  geom_vline(xintercept=as.Date("2020-03-16"), linetype=4) + 
  geom_vline(xintercept=as.Date("2020-03-26"), linetype=4) + 
  geom_vline(xintercept=as.Date("2020-11-05"), linetype=4) + 
  ylab("Household inventory of toilet paper (in rolls)") + ylim(0,48)

zoomdate <- seq(as.Date("2020-01-01"), as.Date("2020-06-30"), by="days")
#ggplot(Inv_plot_input[Inv_plot_input$Date %in% zoomdate,], aes(x=Date, y=Mean)) + geom_line() + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3) + geom_vline(xintercept=as.Date("2020-03-12"), linetype=4) + ylab("Household inventory of toilet paper (in rolls)")
ggplot(Inv_plot_input[Inv_plot_input$Date %in% zoomdate,], aes(x=Date, y=Mean)) + geom_line() + 
  geom_vline(xintercept=as.Date("2020-03-16"), linetype=4) + 
  geom_vline(xintercept=as.Date("2020-03-26"), linetype=4) + 
  geom_vline(xintercept=as.Date("2020-11-05"), linetype=4) + 
  ylab("Household inventory of toilet paper (in rolls)")


# unlist daily inventory 
Inv <- data.frame(ID=rep(names(HH_Inv), each=length(all.dates)), 
                  Date=rep(all.dates, length(panelists_keep_ID_cat)),
                  Inv=unlist(lapply(HH_Inv[1:length(panelists_keep_ID_cat)],"[[",5 ), recursive=F))

  
  
  
#####################################################
# save prepared purchase dataset + inventory variable
#####################################################


if (country == "NL") {TP_purch <- TP_purch[,c("Date_of_purchase","Barcode", "Panelist",
                                           "Banner_name", "Total_unit_sales","Total_value_sales",
                                           "Total_volume_sales", "Quarter", "Year", "Promo","PL",
                                           "ply","sheet", "Purchase_method")] }
save(TP_purch, file=paste(project_path,"/R_files/Preparation/toilet paper/",country,"_purch_toiletpaper.RData",sep=""))
save(Inv, file=paste(project_path,"/R_files/Preparation/toilet paper/",country,"_INV_toiletpaper.RData",sep=""))


