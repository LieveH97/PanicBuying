##############################################
###                                        ###
###       GfK Panel Questionnaire          ###
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

#load libraries - you have to install the packages before running the code!
list.of.packages <- c("plyr","tidyr","ggplot2", "tidyverse", "zoo", "readxl")

#Install package if they are not installed yet
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)




#######################################
# loading the questionnaire responses #
#######################################

GfKresponses <- read.csv(file = paste(data_path,"/NL_COVID19_Phase2.csv",sep=""))

# only keep responses to relevant questions
relevant_questions <- c("q11a_7", "q11a_13", "q15_3", "q16_3", "q17_3", "q18_3", "q19_3")
GfKresponses <- GfKresponses[,c("Panelist",relevant_questions)]

# only keep households we use in our analyses 
load(paste(project_path,"/R_files/INC-QUANT model/toilet paper/FullData_with_moderators_and_labels.RData",sep=""))
relevant_households <- unique(FullData$Household)
rm(FullData)
save(relevant_households, file = paste(project_path,"/R_files/INC-QUANT model/toilet paper/households_in_analysis", sep=""))
GfKresponses <- GfKresponses[GfKresponses$Panelist %in% relevant_households,]
GfKrespondents <- unique(GfKresponses$Panelist)
save(GfKrespondents, file=paste(project_path, "/R_files/Supply-side concerns/GfK-respondents", sep=""))


GfKresponses$q11a_7 <- as.factor(GfKresponses$q11a_7)
GfKresponses$q11a_13 <- as.factor(GfKresponses$q11a_13)
GfKresponses$q15_3 <- as.factor(GfKresponses$q15_3)
GfKresponses$q16_3 <- as.factor(GfKresponses$q16_3)
GfKresponses$q17_3 <- as.factor(GfKresponses$q17_3)
GfKresponses$q18_3 <- as.factor(GfKresponses$q18_3)
GfKresponses$q19_3 <- as.factor(GfKresponses$q19_3)

str(GfKresponses)



###################################
# include questionnaire questions #
###################################

colnames(GfKresponses) <- c("Panelist",
                        "Q11A-7 I have to visit multiple stores because the products I need are sold out at the store I usually visit",
                        "Q11A-13 I buy other brands because my favourite brands are not available",
                        "Q15-3 During this period when restrictions on human contact/public life were imposed, did you buy less or more of the Toilet paper and/or kitchen paper category than before the restrictions took effect?",
                        "Q16-3 Now think about the categories you bought less often than before. Was this because they were not as readily available in the store as they were prior to the restrictions taking effect?",
                        "Q17-3 Now think about the categories you bought more often than before. Was this more because you stocked up on them, because you used more of them or both?",
                        "Q18-3 How does the current stock per product category that you have at home compare to the usual stock that you had at home before the restrictions due to the coronavirus came into force?",
                        "Q19-3 What do you plan to do with the stock of these product categories in the coming weeks?"
)




################################## 
# look into answers regarding TP #
##################################

# Q 11A - 7
ggplot(GfKresponses, aes(x = `Q11A-7 I have to visit multiple stores because the products I need are sold out at the store I usually visit`)) + 
  geom_bar(stat = "count") + scale_x_discrete(labels = c("less often","equally","more often", "not applicable"))

# Q 11A - 13
ggplot(GfKresponses, aes(x =  `Q11A-13 I buy other brands because my favourite brands are not available`)) + 
  geom_bar(stat = "count") + scale_x_discrete(labels = c("less often","equally","more often", "not applicable"))

# Q15 - 3
ggplot(GfKresponses, aes(x = `Q15-3 During this period when restrictions on human contact/public life were imposed, did you buy less or more of the Toilet paper and/or kitchen paper category than before the restrictions took effect?`)) +
  geom_bar(stat = "count") + scale_x_discrete(labels = c("I didn't normally buy at all during the restrictions","much less often than before","somewhat less often than before","as often as before", "somewhat more often than before", "much more often than before"))

# Q16 - 3
ggplot(GfKresponses, aes(x = `Q16-3 Now think about the categories you bought less often than before. Was this because they were not as readily available in the store as they were prior to the restrictions taking effect?`)) +
  geom_bar(stat = "count") + scale_x_discrete(labels = c("yes", "no"))

# Q17 - 3
ggplot(GfKresponses, aes(x = `Q17-3 Now think about the categories you bought more often than before. Was this more because you stocked up on them, because you used more of them or both?`)) +
  geom_bar(stat = "count") + scale_x_discrete(labels = c("just to punch in","mainly to store","both to store and for increased use","mainly because of increased use","only because of increased use"))

# Q18 - 3
ggplot(GfKresponses, aes(x = `Q18-3 How does the current stock per product category that you have at home compare to the usual stock that you had at home before the restrictions due to the coronavirus came into force?`)) + 
  geom_bar(stat = "count") + scale_x_discrete(labels= c("I didn't usually have any stock and I don't have any now","much smaller inventory","slightly smaller stock","same stock", "slightly larger stock","much larger stock"))

# Q19 - 3
ggplot(GfKresponses, aes( x = `Q19-3 What do you plan to do with the stock of these product categories in the coming weeks?`)) + 
  geom_bar(stat = "count") + scale_x_discrete(labels = c("significantly reduce","slightly reduce","sustain","slightly increase","considerably increase"))







################################################
# Filtering Households based on survey answers #
################################################

"Q11A-7-exclude-more-often" <- GfKresponses$Panelist[!GfKresponses$`Q11A-7 I have to visit multiple stores because the products I need are sold out at the store I usually visit` == 3 |
                                                       is.na(GfKresponses$`Q11A-7 I have to visit multiple stores because the products I need are sold out at the store I usually visit`)]
save("Q11A-7-exclude-more-often", file=paste(project_path,"/R_files/Supply-side concerns/Q11A-7-exclude-more-often",sep=""))

"Q16-3-only-NAs" <- GfKresponses$Panelist[is.na(GfKresponses$`Q16-3 Now think about the categories you bought less often than before. Was this because they were not as readily available in the store as they were prior to the restrictions taking effect?`)]
save("Q16-3-only-NAs", file=paste(project_path, "/R_files/Supply-side concerns/Q16-3-only-NAs", sep=""))

"Q16-3-exclude-less-available" <- GfKresponses$Panelist[!GfKresponses$`Q16-3 Now think about the categories you bought less often than before. Was this because they were not as readily available in the store as they were prior to the restrictions taking effect?` == 1 |
                                                          is.na(GfKresponses$`Q16-3 Now think about the categories you bought less often than before. Was this because they were not as readily available in the store as they were prior to the restrictions taking effect?`)]
save("Q16-3-exclude-less-available" , file=paste(project_path, "/R_files/Supply-side concerns/Q16-3-exclude-less-available", sep=""))











