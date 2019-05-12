#Loading required packages

rqd_pkg <-
  c(
    "MASS",
    "car",
    "dplyr",
    "stringdist",
    "stringr",
    "forcats",
    "tidyr",
    "tidyverse",
    "lubridate",
    "ggplot2",
    "reshape2",
    "caret",
    "e1071",
    "ROCR",
    "ROSE",
    "scales",
    "Information",
    "magrittr",
    "AtConP",
    "cowplot",
    "caTools"
  )
pload <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
pload(rqd_pkg)

directory <- getwd()

#credit_data<- cred
#demog_data<- dem
  
#Loading data files
credit_data<- read.csv(file.choose()) #("../input/Credit Bureau data.csv")
demog_data<- read.csv(file.choose())  #("../input/Demographic data.csv")

#*******************************************************************************#
# Data Understanding and Data Prepration - Begins
#*******************************************************************************#

view(demog_data)
str(demog_data)
#'data.frame':	71295 obs. of  12 variables:
# $ Application.ID                             : int  954457215 432830445 941387308 392161677 182011211 312196805 532217204 74788849 782743811 96964957 ...
# $ Age                                        : int  48 31 32 43 35 20 42 34 30 22 ...
# $ Gender                                     : Factor w/ 3 levels "","F","M": 2 3 3 3 2 3 3 3 3 3 ...
# $ Marital.Status..at.the.time.of.application.: Factor w/ 3 levels "","Married","Single": 2 2 3 2 2 2 2 2 2 3 ...
# $ No.of.dependents                           : int  2 4 2 1 5 1 2 2 3 1 ...
# $ Income                                     : num  40 55 46 53 44 39 55 49 48 38 ...
# $ Education                                  : Factor w/ 6 levels "","Bachelor",..: 2 6 2 2 6 2 6 3 5 2 ...
# $ Profession                                 : Factor w/ 4 levels "","SAL","SE",..: 2 4 4 3 2 2 2 4 2 4 ...
# $ Type.of.residence                          : Factor w/ 6 levels "","Company provided",..: 6 6 6 6 6 1 6 6 6 6 ...
# $ No.of.months.in.current.residence          : int  113 112 104 94 112 116 104 108 115 111 ...
# $ No.of.months.in.current.company            : int  56 46 49 53 43 52 41 40 58 57 ...
# $ Performance.Tag                            : int  0 0 0 0 0 0 0 0 0 0 ...

length(unique(demog_data$Application.ID)) == nrow(demog_data)
#[1] FALSE
# Shows duplicated Application ID

dem_dupid <- demog_data[duplicated(demog_data$Application.ID),1] #returns application ID details 
dup_dem <- demog_data %>% filter(demog_data$Application.ID %in% dem_dupid )
View(dup_dem) # Shows duplicates have different features

view(credit_data)
str(credit_data)

#data.frame':	71295 obs. of  19 variables:
#$ Application.ID                                                 : int  954457215 432830445 941387308 392161677 182011211 312196805 532217204 74788849 782743811 96964957 ...
#$ No.of.times.90.DPD.or.worse.in.last.6.months                   : int  0 0 0 0 0 0 0 0 0 0 ...
#$ No.of.times.60.DPD.or.worse.in.last.6.months                   : int  0 0 0 0 0 0 0 0 0 0 ...
#$ No.of.times.30.DPD.or.worse.in.last.6.months                   : int  0 0 0 0 0 0 0 0 0 0 ...
#$ No.of.times.90.DPD.or.worse.in.last.12.months                  : int  0 0 0 0 0 0 0 0 0 0 ...
#$ No.of.times.60.DPD.or.worse.in.last.12.months                  : int  0 0 0 0 0 0 0 0 0 0 ...
#$ No.of.times.30.DPD.or.worse.in.last.12.months                  : int  0 0 0 0 0 0 0 0 1 0 ...
#$ Avgas.CC.Utilization.in.last.12.months                         : int  4 3 7 11 12 10 11 13 9 6 ...
#$ No.of.trades.opened.in.last.6.months                           : int  1 1 0 1 0 0 0 1 0 1 ...
#$ No.of.trades.opened.in.last.12.months                          : int  2 2 0 1 1 0 1 1 0 1 ...
#$ No.of.PL.trades.opened.in.last.6.months                        : int  0 0 0 0 0 0 0 0 0 0 ...
#$ No.of.PL.trades.opened.in.last.12.months                       : int  0 0 0 0 0 0 0 0 0 0 ...
#$ No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. : int  0 0 0 0 0 0 0 0 0 0 ...
#$ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.: int  0 0 0 0 0 0 0 0 0 0 ...
#$ Presence.of.open.home.loan                                     : int  1 0 1 1 1 0 1 1 1 0 ...
#$ Outstanding.Balance                                            : int  2999395 3078 3004972 3355373 3014283 2569 3005535 3004790 3007428 170860 ...
#$ Total.No.of.Trades                                             : int  4 5 2 4 4 1 4 3 2 1 ...
#$ Presence.of.open.auto.loan                                     : int  0 0 0 1 0 0 0 0 0 1 ...
#$ Performance.Tag                                                : int  0 0 0 0 0 0 0 0 0 0 ...

length(unique(credit_data$Application.ID)) == nrow(credit_data)
#[1] FALSE
# Shows duplicated Application ID

cred_dupid <- credit_data[duplicated(credit_data$Application.ID),1] #returns application ID details

identical(dem_dupid,cred_dupid)
#TRUE

identical(demog_data$Application.ID,credit_data$Application.ID) #Check if both datasets have common Application IDs
#TRUE

dup_cred <- credit_data %>% filter(credit_data$Application.ID %in% cred_dupid)
View(dup_cred) # Shows duplicates have different features

#Lets sort and then add row_ids to all Appids so we can make them unique 
demog_data <- demog_data[order(demog_data$Application.ID),]
credit_data <- credit_data[order(credit_data$Application.ID),]

demog_data <- mutate(demog_data, Rid = rownames(demog_data))
credit_data <- mutate(credit_data, Rid = rownames(credit_data))

demog_data <- demog_data %>% unite_("NewID",c("Rid","Application.ID"),remove = TRUE)
credit_data <- credit_data %>% unite_("NewID",c("Rid","Application.ID"),remove = TRUE)
identical(demog_data1$New.ID,credit_data1$New.ID)
#TRUE - shows that we have preserved integrity

demog_data[duplicated(demog_data$Application.ID),1] #character(0)
credit_data[duplicated(credit_data$Application.ID),1] #character(0)

dim(demog_data) #71295  12
dim(credit_data)#71295  19

data.frame(colSums(is.na(demog_data)))
# colSums.is.na.demog_data..
# NewID                                                                0
# Age                                                                  0
# Gender                                                               0
# Marital.Status..at.the.time.of.application.                          0
# No.of.dependents                                                     3
# Income                                                               0
# Education                                                            0
# Profession                                                           0
# Type.of.residence                                                    0
# No.of.months.in.current.residence                                    0
# No.of.months.in.current.company                                      0
# Performance.Tag                                                   1425

data.frame(colSums(is.na(credit_data)))
# colSums.is.na.credit_data..
# NewID                                                                                     0
# No.of.times.90.DPD.or.worse.in.last.6.months                                              0
# No.of.times.60.DPD.or.worse.in.last.6.months                                              0
# No.of.times.30.DPD.or.worse.in.last.6.months                                              0
# No.of.times.90.DPD.or.worse.in.last.12.months                                             0
# No.of.times.60.DPD.or.worse.in.last.12.months                                             0
# No.of.times.30.DPD.or.worse.in.last.12.months                                             0
# Avgas.CC.Utilization.in.last.12.months                                                 1058
# No.of.trades.opened.in.last.6.months                                                      1
# No.of.trades.opened.in.last.12.months                                                     0
# No.of.PL.trades.opened.in.last.6.months                                                   0
# No.of.PL.trades.opened.in.last.12.months                                                  0
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.                            0
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.                           0
# Presence.of.open.home.loan                                                              272
# Outstanding.Balance                                                                     272
# Total.No.of.Trades                                                                        0
# Presence.of.open.auto.loan                                                                0
# Performance.Tag                                                                        1425

#1425 Performance.Tag (predicted variable) NA in demog_data and credit_data - need to check commonality

demogNAPerformance.Tag <- demog_data[is.na(demog_data$NewID),]
creditNAPerformance.Tag <- credit_data[is.na(credit_data$NewID),]
identical(demogNAPerformance.Tag$NewID, creditNAPerformance.Tag$NewID)
# [1] TRUE so we can delete the Performance.Tag NA values in both datasets

#removing NA values for Performance.Tag
demog_data <- demog_data[!is.na(demog_data$Performance.Tag),]
credit_data <- credit_data[!is.na(credit_data$Performance.Tag),]

#We have now removed ~2% of data 

data.frame(lengths(lapply(demog_data,unique)))

#App_ID                    69870        Non duplicated values (unique)                      
#Age                          53        Check for negative value, <18                      
#Gender                        3        Factor var check                      
#MaritalStatus                 3        Factor var check                      
#No_of_dependents              6        Check for negative value                      
#Income                       63        Check for negative value                      
#Education                     6        Factor var check                      
#Profession                    4        Factor var check                      
#Residence_type                6        Factor var check                              
#No_of_mons_curr_residence   121        Check for negatives                          
#No_of_mons_curr_company      83        Check for negatives                      
#Performance_Tag               2        Factor var check

summary(demog_data$Age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3      37      45      45      53      65 
# Min age is negative which is misleading - Min age should be greater than 18 for credit card. need to check for outliers

nrow(subset(demog_data, Age < "18"))
#[1] 65
#65 data points have Age less than 18 which appears suspect for credit card issuance

levels(demog_data$Gender)
# ""  "F" "M"

data.frame(summary(demog_data$Gender))
#  summary.demog_data.Gender. 
#          2
# F    16506
# M    53362
# shows 2 missing values 

levels(demog_data$Marital.Status..at.the.time.of.application.)
#""        "Married" "Single"

data.frame(summary(demog_data$Marital.Status..at.the.time.of.application.))
#             6
# Married 59547
# Single  10317
# shows 6 missing values 

table(demog_data$Marital.Status..at.the.time.of.application.,demog_data$Age <18, useNA = "ifany")
#             FALSE  TRUE
#                5     1
# Married     59507    40
# Single      10293    24

#Suggests that Age < 18 has spurious data and should be excluded

demog_data <- demog_data[demog_data$Age >=18,] #Excluding datapoints where age <= 18

#Excluding data points where Marital Status is blank

demog_data <- demog_data[!(demog_data$Marital.Status..at.the.time.of.application.== ""),] #Excluding bad data for No of dependents
demog_data$Marital.Status..at.the.time.of.application. <- factor(demog_data$Marital.Status..at.the.time.of.application.)

summary(demog_data$No.of.dependents)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.00    2.00    3.00    2.86    4.00    5.00       2 

length(which(is.na(demog_data$No.of.dependents)))
#[1] 2

demog_data <- demog_data[!is.na(demog_data$No.of.dependents),] #Removing NA values for dependents

table(demog_data$Marital.Status..at.the.time.of.application.,demog_data$No.of.dependents, useNA = "ifany")
#                 1     2     3     4     5
#                 1     1     1     1     1
# Married     12567 12586 13102 10678 10572
# Single       2632  2518  2524  1318  1301

demog_data <- demog_data[!(demog_data$Gender== ""),] #Excluding bad data for Gender
demog_data$Gender <- factor(demog_data$Gender)

summary(demog_data$Income)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.50   14.00   27.00   27.41   40.00   60.00

length(which(demog_data$Income <= "0"))
#[1] 106
# shows 106 income values less than equal to zero. They should be excluded 

demog_data <- demog_data[demog_data$Income > 0,]
table(demog_data$Performance.Tag,demog_data$Income <= 0)
#   FALSE
# 0 66746
# 1  2945

data.frame(summary(demog_data$Education))
#                                        118
# Bachelor                             17265
# Masters                              23430
# Others                                 118
# Phd                                   4453
# Professional                         24307

# removing blanks

demog_data <- demog_data[!(demog_data$Education == ""),]
demog_data$Education <- factor(demog_data$Education)


data.frame(summary(demog_data$Profession))
# summary.demog_data.Profession.
#                                     11  Remove blanks
# SAL                              39514
# SE                               13875
# SE_PROF                          16173

demog_data <- demog_data[!(demog_data$Profession == ""),]
demog_data$Profession <- factor(demog_data$Profession)

data.frame(summary(demog_data$Type.of.residence))
# summary.demog_data.Type.of.residence.
#                                                         8   Remove blanks
# Company provided                                     1595
# Living with Parents                                  1757
# Others                                                197
# Owned                                               13928
# Rented                                              52077

demog_data <- demog_data[!(demog_data$Type.of.residence == ""),]
demog_data$Type.of.residence <- factor(demog_data$Type.of.residence)

summary(demog_data$No.of.months.in.current.residence)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00    6.00   10.00   34.47   60.00  126.00
# Seems OK

summary(demog_data$No.of.months.in.current.company)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00   16.00   34.00   34.18   51.00  133.00 
# Seems OK

#Now to match the credit_data with IDs that are there demog_data
matchingNewID <- demog_data$NewID
credit_data_matched <- credit_data %>% filter(credit_data$NewID %in% matchingNewID)

identical(demog_data$NewID,credit_data_matched$NewID)
#Confirmed TRUE


