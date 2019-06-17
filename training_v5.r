#================CAPSTONE PROJECT====================

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
    "caTools",
    "woeBinning",
    "kernlab",
    "rpart.plot",
    "rpart",
    "rattle",
    "randomForest"
  )
pload <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
pload(rqd_pkg)

directory <- getwd()


#Loading data files
credit_data<- read.csv(file.choose()) #("../input/Credit Bureau data.csv")
demog_data<- read.csv(file.choose())  #("../input/Demographic data.csv")


#*******************************************************************************#
# Data Understanding and Data Prepration - Begins
#*******************************************************************************#

View(demog_data)
str(demog_data)

#'data.frame':	71295 obs. of  12 variables:
#  $ Application.ID                             : int  954457215 432830445 941387308 392161677 182011211 312196805 532217204 74788849 782743811 96964957 ...
#$ Age                                        : int  48 31 32 43 35 20 42 34 30 22 ...
#$ Gender                                     : Factor w/ 3 levels "","F","M": 2 3 3 3 2 3 3 3 3 3 ...
#$ Marital.Status..at.the.time.of.application.: Factor w/ 3 levels "","Married","Single": 2 2 3 2 2 2 2 2 2 3 ...
#$ No.of.dependents                           : int  2 4 2 1 5 1 2 2 3 1 ...
#$ Income                                     : num  40 55 46 53 44 39 55 49 48 38 ...
#$ Education                                  : Factor w/ 6 levels "","Bachelor",..: 2 6 2 2 6 2 6 3 5 2 ...
#$ Profession                                 : Factor w/ 4 levels "","SAL","SE",..: 2 4 4 3 2 2 2 4 2 4 ...
#$ Type.of.residence                          : Factor w/ 6 levels "","Company provided",..: 6 6 6 6 6 1 6 6 6 6 ...
#$ No.of.months.in.current.residence          : int  113 112 104 94 112 116 104 108 115 111 ...
#$ No.of.months.in.current.company            : int  56 46 49 53 43 52 41 40 58 57 ...
#$ Performance.Tag                            : int  0 0 0 0 0 0 0 0 0 0 ...

View(credit_data)
str(credit_data)

#'data.frame':	71295 obs. of  19 variables:
#  $ Application.ID                                                 : int  954457215 432830445 941387308 392161677 182011211 312196805 532217204 74788849 782743811 96964957 ...
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

#-----------------------
#Checking NA values in the dataset
#-----------------------
colnames(demog_data)[colSums(is.na(demog_data)) > 0]
#"No.of.dependents" "Performance.Tag" 

colnames(credit_data)[colSums(is.na(credit_data)) > 0]
#"Avgas.CC.Utilization.in.last.12.months" "No.of.trades.opened.in.last.6.months"   "Presence.of.open.home.loan"            
# "Outstanding.Balance"                    "Performance.Tag"

#-----------------------
#Checking for duplicate Application IDs
#-----------------------
length(unique(demog_data$Application.ID)) == nrow(demog_data)
#FALSE - Contains duplicate Application IDs

length(unique(credit_data$Application.ID)) == nrow(credit_data)
#FALSE - Contains duplicate Application IDs

#Grabbing the Duplicate Application IDs in demographic data
dem_dupid <- demog_data[duplicated(demog_data$Application.ID),1] #returns application ID details 
dem_dupid
#765011468 653287861 671989187 are the duplicate Application IDs in demographic dataset

#Grabbing the rows with duplicate Application IDs in Demographic data
dup_dem <- demog_data %>% filter(demog_data$Application.ID %in% dem_dupid )
View(dup_dem) # Shows duplicates have different features in Demographic data

#Grabbing the Duplicate Application IDs in Credit beuraeu data
credit_dupid <- credit_data[duplicated(credit_data$Application.ID),1] #returns application ID details 
credit_dupid
#765011468 653287861 671989187 are the duplicate Application IDs in Credit beuraeu data

#Grabbing the rows with duplicate Application IDs in Credit beuraeu data
dup_credit <- credit_data %>% filter(credit_data$Application.ID %in% credit_dupid )
View(dup_credit) # Shows duplicates have different features in Credit beuraeu data

#Checking if the same Application IDs are duplicated in both the data set
identical(dem_dupid,credit_dupid)
#TRUE - Both dataset have same set of duplicated Application IDs


#Remvoing Rows with Duplicate Application IDs

demog_data <- demog_data[!(duplicated(demog_data$Application.ID)),]
credit_data <- credit_data[!(duplicated(credit_data$Application.ID)),]


#-----------------------
# Insight on NA values
#-----------------------

data.frame(colSums(is.na(demog_data)))

#Application.ID                                                       0
#Age                                                                  0
#Gender                                                               0
#Marital.Status..at.the.time.of.application.                          0
#No.of.dependents                                                     3
#Income                                                               0
#Education                                                            0
#Profession                                                           0
#Type.of.residence                                                    0
#No.of.months.in.current.residence                                    0
#No.of.months.in.current.company                                      0
#Performance.Tag                                                   1425


data.frame(colSums(is.na(credit_data)))

#Application.ID                                                                            0
#No.of.times.90.DPD.or.worse.in.last.6.months                                              0
#No.of.times.60.DPD.or.worse.in.last.6.months                                              0
#No.of.times.30.DPD.or.worse.in.last.6.months                                              0
#No.of.times.90.DPD.or.worse.in.last.12.months                                             0
#No.of.times.60.DPD.or.worse.in.last.12.months                                             0
#No.of.times.30.DPD.or.worse.in.last.12.months                                             0
#Avgas.CC.Utilization.in.last.12.months                                                 1058
#No.of.trades.opened.in.last.6.months                                                      1
#No.of.trades.opened.in.last.12.months                                                     0
#No.of.PL.trades.opened.in.last.6.months                                                   0
#No.of.PL.trades.opened.in.last.12.months                                                  0
#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.                            0
#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.                           0
#Presence.of.open.home.loan                                                              272
#Outstanding.Balance                                                                     272
#Total.No.of.Trades                                                                        0
#Presence.of.open.auto.loan                                                                0
#Performance.Tag                                                                        1425


#1425 Performance.Tag (predicted variable) NA in demog_data and credit_data - need to check commonality
demogNAPerformance.Tag <- demog_data[is.na(demog_data$NewID),]
creditNAPerformance.Tag <- credit_data[is.na(credit_data$NewID),]
identical(demogNAPerformance.Tag$NewID, creditNAPerformance.Tag$NewID)
# TRUE so we can delete the Performance.Tag NA values in both datasets

#removing NA values for Performance.Tag
demog_data <- demog_data[!is.na(demog_data$Performance.Tag),]
credit_data <- credit_data[!is.na(credit_data$Performance.Tag),]

#-----------------------
# EDA - to understand data and find data issues if any
#-----------------------

#DEMOGRAPHIC DATA

#Checking levels in the demographic dataset
data.frame(lengths(lapply(demog_data,unique)))

#Application.ID                                                            69867
#Age                                                                          53
#Gender                                                                        3
#Marital.Status..at.the.time.of.application.                                   3
#No.of.dependents                                                              6
#Income                                                                       63
#Education                                                                     6
#Profession                                                                    4
#Type.of.residence                                                             6
#No.of.months.in.current.residence                                           121
#No.of.months.in.current.company                                              83
#Performance.Tag                                                               2

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

demog_data <- demog_data[!(demog_data$Marital.Status..at.the.time.of.application.== ""),] 
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

#Excluding bad data for Gender
demog_data <- demog_data[!(demog_data$Gender== ""),] 
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
matchingIDs <- demog_data$Application.ID
credit_data_matched <- credit_data %>% filter(credit_data$Application.ID %in% matchingIDs)

identical(demog_data$Application.ID,credit_data_matched$Application.ID)
#Confirmed TRUE -  credit_data_matched data set has the same set of Application IDs as demographic dataset


###################################################
# Variable wise analysis for all variable for Demographic data
##################################################


# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag~cat_var, demog_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_default <- cbind(a, count)
  
  colnames(agg_default) <- c(var_name, "default_rate","No.of_IDS")
  agg_default[, 2] <- format(round(agg_default[, 2], 3))
  
  ggplot(agg_default, aes(agg_default[, 1], default_rate, label = count)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}


#Age 

table(demog_data$Age) 
#18   19   20   21   22   23   24   25   26   27   28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43 
#23   23   27   13   23   35  107   97   93 1378 1329 1321 1380 1318 1440 1364 1321 1441 2259 2336 2307 2480 2340 2283 2348 2356 

#44   45   46   47   48   49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64   65 
#2275 2223 2294 2290 2263 2215 2242 2315 2258 2248 2259 2168 2178  998 1048  990 1032 1006  984  964  914  948 

summary(demog_data$Age) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#18.00   38.00   45.00   45.04   53.00   65.00

histogram(demog_data$Age) 
boxplot(demog_data$Age,ylab = "Age",col = "blue")

plot_response(demog_data$Age, "Age")
#Default rate is highest for the 55-65 age bracket, followed by 35-45 amd 25-35
#No of applicants is highest in the 35-45 bracket and lowest in 55-65 bracket


#********************************************************************#


#Gender 
table(demog_data$Gender) 
#F     M 
#16442 53112

plot_response(demog_data$Gender, "Gender")
#Women have higher default rate than men

#********************************************************************#
#Marital.Status..at.the.time.of.application. 

table(demog_data$Marital.Status..at.the.time.of.application.) 
#Married  Single 
#59295   10259
histogram(demog_data$Marital.Status..at.the.time.of.application.) 

plot_response(demog_data$Marital.Status..at.the.time.of.application., "Marital Status")
#marginally higher default among single applicants


#********************************************************************#
#No.of.dependents 

table(demog_data$No.of.dependents) 
#1     2     3     4     5 
#15144 15060 15565 11953 11832

summary(demog_data$No.of.dependents) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00    2.00    3.00    2.86    4.00    5.00 

histogram(demog_data$No.of.dependents) 
plot_response(demog_data$No.of.dependents, "Dependents")
# higher default for more than 2 dependents

#********************************************************************#
#Income 

boxplot(demog_data$Income,ylab = "Income",col = "blue") #No outliers

summary(demog_data$Income) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   14.00   27.00   27.45   40.00   60.00

quantile(demog_data$Income,seq(0,1,0.01))
# 0%   1%   2%   3%   4%   5%   6%   7%   8%   9%  10%  11%  12%  13%  14%  15%  16%  17%  18%  19%  20%  21%  22%  23%  24% 
# 1.0  4.5  4.5  4.5  4.5  4.5  4.5  4.5  5.0  6.0  6.0  7.0  7.0  8.0  8.0  9.0  9.0 10.0 10.0 11.0 11.0 12.0 13.0 13.0 14.0 
# 25%  26%  27%  28%  29%  30%  31%  32%  33%  34%  35%  36%  37%  38%  39%  40%  41%  42%  43%  44%  45%  46%  47%  48%  49% 
# 14.0 15.0 15.0 16.0 16.0 17.0 17.0 18.0 18.0 19.0 19.0 20.0 20.0 21.0 21.0 22.0 22.0 23.0 23.0 24.0 24.0 25.0 25.0 26.0 26.0 
# 50%  51%  52%  53%  54%  55%  56%  57%  58%  59%  60%  61%  62%  63%  64%  65%  66%  67%  68%  69%  70%  71%  72%  73%  74% 
# 27.0 27.0 28.0 29.0 29.0 30.0 30.0 31.0 31.0 32.0 32.0 33.0 33.0 34.0 34.0 35.0 35.0 36.0 36.0 37.0 37.0 38.0 38.0 39.0 39.0 
# 75%  76%  77%  78%  79%  80%  81%  82%  83%  84%  85%  86%  87%  88%  89%  90%  91%  92%  93%  94%  95%  96%  97%  98%  99% 
# 40.0 40.0 41.0 41.0 42.0 42.0 43.0 43.0 44.0 45.0 45.0 46.0 46.0 47.0 48.0 49.0 50.0 51.0 52.0 53.0 54.0 55.0 56.0 58.0 59.0 
# 100% 
# 60.0 

plot_response(demog_data$Income, "Income")
#Lowest income range has highest default rates and default rate progressively reduces

#********************************************************************#
#Education 

table(demog_data$Education) 
#Bachelor      Masters       Others          Phd Professional 
#17260        23425          117         4452        24300  

plot_response(demog_data$Education, "Education")
#Default rate is highest when Education level is "Others", but sample is small
# Phds and professionals have lower default rates but no substantial variance by education


#********************************************************************#
#Profession 

table(demog_data$Profession) 
#SAL      SE SE_PROF 
#39511   13873   16170 

plot_response(demog_data$Profession, "Profession")
#default rates are higher for self employed non professionals

#********************************************************************#
#Type.of.residence 

table(demog_data$Type.of.residence) 
#Company provided Living with Parents              Others               Owned              Rented 
#1595                1757                 197               13928               52077

plot_response(demog_data$Type.of.residence, "Type.of.residence")
#default rates are higher when residence of applicant is Parent/Company provided

#********************************************************************#
#No.of.months.in.current.residence 


summary(demog_data$No.of.months.in.current.residence) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6.00    6.00   10.00   34.47   60.00  126.00 

boxplot(demog_data$No.of.months.in.current.residence,ylab = "No.of.months.in.current.residence",col = "blue") #No outliers

summary(demog_data$No.of.months.in.current.residence) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00    6.00   10.00   34.47   60.00  126.00
#Median no of months in current residence is only 10 - suggesting recency of home

quantile(demog_data$No.of.months.in.current.residence,seq(0,1,0.01))
# 0%   1%   2%   3%   4%   5%   6%   7%   8%   9%  10%  11%  12%  13%  14%  15%  16%  17%  18%  19%  20%  21%  22%  23%  24% 
# 6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6 
# 25%  26%  27%  28%  29%  30%  31%  32%  33%  34%  35%  36%  37%  38%  39%  40%  41%  42%  43%  44%  45%  46%  47%  48%  49% 
# 6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    6    8 
# 50%  51%  52%  53%  54%  55%  56%  57%  58%  59%  60%  61%  62%  63%  64%  65%  66%  67%  68%  69%  70%  71%  72%  73%  74% 
# 10   12   14   15   17   19   21   23   25   27   29   31   33   35   37   39   41   43   45   47   49   51   54   56   58 
# 75%  76%  77%  78%  79%  80%  81%  82%  83%  84%  85%  86%  87%  88%  89%  90%  91%  92%  93%  94%  95%  96%  97%  98%  99% 
# 60   63   65   67   70   72   75   77   80   82   85   88   90   93   95   98  100  103  105  107  110  113  115  118  122 
# 100% 
# 126

plot_response(demog_data$No.of.months.in.current.residence , "Number of months in current residence")
#Default rate higher when residential tenor is less than 4.5 years


#********************************************************************#
#No.of.months.in.current.company 

summary(demog_data$No.of.months.in.current.company) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.00   16.00   34.00   34.18   51.00  133.00
#A large % of the Applications have less than 3 years in current company

quantile(demog_data$No.of.months.in.current.company,seq(0,1,0.01))
# 0%     1%     2%     3%     4%     5%     6%     7%     8%     9%    10%    11%    12%    13%    14%    15%    16%    17% 
# 3.00   3.00   3.00   3.00   3.00   3.00   3.00   4.00   4.00   5.00   6.00   7.00   7.00   8.00   9.00   9.00  10.00  11.00 
# 18%    19%    20%    21%    22%    23%    24%    25%    26%    27%    28%    29%    30%    31%    32%    33%    34%    35% 
# 12.00  12.00  13.00  14.00  14.00  15.00  16.00  16.00  17.00  18.00  19.00  19.00  20.00  21.00  21.00  22.00  23.00  24.00 
# 36%    37%    38%    39%    40%    41%    42%    43%    44%    45%    46%    47%    48%    49%    50%    51%    52%    53% 
# 24.00  25.00  26.00  26.00  27.00  28.00  28.00  29.00  30.00  30.00  31.00  32.00  33.00  33.00  34.00  35.00  35.00  36.00 
# 54%    55%    56%    57%    58%    59%    60%    61%    62%    63%    64%    65%    66%    67%    68%    69%    70%    71% 
# 37.00  37.00  38.00  39.00  39.00  40.00  41.00  41.00  42.00  43.00  43.00  44.00  45.00  45.00  46.00  47.00  48.00  48.00 
# 72%    73%    74%    75%    76%    77%    78%    79%    80%    81%    82%    83%    84%    85%    86%    87%    88%    89% 
# 49.00  50.00  50.00  51.00  52.00  52.00  53.00  54.00  54.00  55.00  56.00  56.00  57.00  58.00  58.00  59.00  60.00  61.00 
# 90%    91%    92%    93%    94%    95%    96%    97%    98%    99%   100% 
# 62.00  62.00  64.00  65.00  66.82  68.00  69.00  71.00  72.00  74.00 133.00 


plot_response(demog_data$No.of.months.in.current.company, "Number of months in current company")



#********************************************************************#
#Creating dummy variables for Demog data
#********************************************************************#

demog_data_with_dummy <- demog_data

#converting dummy variable for factors with more than 2 levels and attaching it to DF

# Create the dummy variable 
gender_dummy <- data.frame(model.matrix( ~Gender, data = demog_data_with_dummy))
gender_dummy <- gender_dummy[,-1]

# Combine the dummy variables and the numeric columns
coloumnNum <- which( colnames(demog_data_with_dummy)=="Gender" )
demog_data_with_dummy <- cbind(demog_data_with_dummy[,-coloumnNum], gender_dummy)


# Create the dummy variable 
marital_status_dummy <- data.frame(model.matrix( ~Marital.Status..at.the.time.of.application., data = demog_data_with_dummy))
marital_status_dummy <- marital_status_dummy[,-1]

# Combine the dummy variables and the numeric columns
coloumnNum <- which( colnames(demog_data_with_dummy)=="Marital.Status..at.the.time.of.application." )
demog_data_with_dummy <- cbind(demog_data_with_dummy[,-coloumnNum], marital_status_dummy)

# Create the dummy variable 
dummy <- data.frame(model.matrix( ~Education, data = demog_data_with_dummy))
dummy <- dummy[,-1]

# Combine the dummy variables and the numeric columns
coloumnNum <- which( colnames(demog_data_with_dummy)=="Education" )
demog_data_with_dummy <- cbind(demog_data_with_dummy[,-coloumnNum], dummy)

# Create the dummy variable 
dummy <- data.frame(model.matrix( ~Profession, data = demog_data_with_dummy))
dummy <- dummy[,-1]

# Combine the dummy variables and the numeric columns
coloumnNum <- which( colnames(demog_data_with_dummy)=="Profession" )
demog_data_with_dummy <- cbind(demog_data_with_dummy[,-coloumnNum], dummy)

# Create the dummy variable 
dummy <- data.frame(model.matrix( ~Type.of.residence, data = demog_data_with_dummy))
dummy <- dummy[,-1]

# Combine the dummy variables and the numeric columns
coloumnNum <- which( colnames(demog_data_with_dummy)=="Type.of.residence" )
demog_data_with_dummy <- cbind(demog_data_with_dummy[,-coloumnNum], dummy)



#********************************************************************#
# Modelling on Demographic Data for predicting Perforamce Tag
#********************************************************************#

#Finding distribution of Perforance Tag values oin the dataset

nrow(demog_data_with_dummy[which(demog_data_with_dummy$Performance.Tag == 1),]) /
  nrow(demog_data_with_dummy)
# 0.04225676 - around 4 percent of the dataset is default case, The data set is skewed




#Splitting data between test and train
#splitting data into train and test
set.seed(123)

split_indices <- sample.split(demog_data_with_dummy$Performance.Tag, SplitRatio = 0.70)

train <- demog_data_with_dummy[split_indices, ]

test <- demog_data_with_dummy[!split_indices, ]

nrow(train)/nrow(demog_data_with_dummy)

nrow(test)/nrow(demog_data_with_dummy)

table(train$Performance.Tag) 
table(test$Performance.Tag) 


#**************************************************************************************************#
#***** Since Performance tag is very skewed. Taking measures to avoid bias ************************#
#**************************************************************************************************#
#segregating non-default datapoint into appropriate number of clusters and pick 2000 points 
#from each of the clusters to downscale the amount of non-default data-points using stratified sampling
#and replicate the default cases to increase the amount of default cases in the dataset

# segregating default and non default cases
demog_data_with_dummy_Default <- train[which(train$Performance.Tag == 1),]
demog_data_with_dummy_nonDefault <- train[which(train$Performance.Tag == 0),]


#Confirming no NAs before clustering
colnames(demog_data_with_dummy_nonDefault)[colSums(is.na(demog_data_with_dummy_nonDefault)) > 0]
colSums(is.na(demog_data_with_dummy_nonDefault))
str(demog_data_with_dummy_nonDefault)
r_sq<- rnorm(20)


#Cheking for the right number of clusters possible on non-default data set
for (number in 1:20){
  clus <- kmeans(demog_data_with_dummy_nonDefault[,-1], centers = number, nstart = 50)
  r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)

# 5 clusters seems to be a decent cluster size
## Running the K-Means algorithm for K =5

#Clustering non-default data set into 5 clusters

clus5 <- kmeans(demog_data_with_dummy_nonDefault[,-1], centers = 5, iter.max = 50, nstart = 50)

## Appending the ClusterIDs to RFM data

non_default_data <-cbind(demog_data_with_dummy_nonDefault,clus5$cluster)

colnames(non_default_data)[20]<- "ClusterID"

#taking out 2000 random data points from each cluster formed on non-default data set
cluster1 <- non_default_data[which(non_default_data$ClusterID == "1"),]
cluster2 <- non_default_data[which(non_default_data$ClusterID == "2"),]
cluster3 <- non_default_data[which(non_default_data$ClusterID == "3"),]
cluster4 <- non_default_data[which(non_default_data$ClusterID == "4"),]
cluster5 <- non_default_data[which(non_default_data$ClusterID == "5"),]

downscale_non_default <- rbind(cluster1[sample(nrow(cluster1), 2000), ],
                               cluster2[sample(nrow(cluster2), 2000), ],
                               cluster3[sample(nrow(cluster3), 2000), ],
                               cluster4[sample(nrow(cluster4), 2000), ],
                               cluster5[sample(nrow(cluster5), 2000), ])

#combining the deafulat data set with the randomly selected 10000 non-default data sets
#replicating the default cases to increase the amount of default data-points in the data-set
final_data_set <- rbind(downscale_non_default[,-20], demog_data_with_dummy_Default)
final_data_set <- rbind(final_data_set, demog_data_with_dummy_Default)
final_data_set <- rbind(final_data_set, demog_data_with_dummy_Default)
final_data_set <- rbind(final_data_set, demog_data_with_dummy_Default)

#After treatment, checking the distribution of data set
nrow(final_data_set[which(final_data_set$Performance.Tag == 1),]) /
  nrow(final_data_set)
# 0.4513935 - around 45% of the data set is default case

# removing Application.ID as it is irrelevant to the Model

final_data_set <- final_data_set[ , !(names(final_data_set) %in% c("Application.ID"))]


#changing Preformance tag to factor
final_data_set$Performance.Tag <- as.factor(ifelse(final_data_set$Performance.Tag == 1, "yes", "no"))
test$Performance.Tag <- as.factor(ifelse(test$Performance.Tag == 1, "yes", "no"))


#********************************************************************#
#Training a Logistic model on Demographic data set
#********************************************************************#

logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = final_data_set)

summary(logistic_1)
#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")

summary(logistic_2)

# stepAIC has removed some variables and only the following ones remain

logistic_3 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + EducationOthers + ProfessionSE + 
                    Type.of.residenceLiving.with.Parents + Type.of.residenceOthers + 
                    Type.of.residenceOwned + Type.of.residenceRented, family = "binomial", data = final_data_set)

summary(logistic_3)
vif(logistic_3)

#-----------------------------------------------------------
#removing Type.of.residenceRented
logistic_4 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + EducationOthers + ProfessionSE + 
                    Type.of.residenceLiving.with.Parents + Type.of.residenceOthers + 
                    Type.of.residenceOwned , family = "binomial", data = final_data_set)

summary(logistic_4)
vif(logistic_4)

#-----------------------------------------------------------
#removing Type.of.residenceOwned

logistic_5 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + EducationOthers + ProfessionSE + 
                    Type.of.residenceLiving.with.Parents + Type.of.residenceOthers 
                  , family = "binomial", data = final_data_set)

summary(logistic_5)
vif(logistic_5)

#-----------------------------------------------------------
#removing Type.of.residenceLiving.with.Parents

logistic_6 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + EducationOthers + ProfessionSE + 
                    Type.of.residenceOthers 
                  , family = "binomial", data = final_data_set)

summary(logistic_6)
vif(logistic_6)

#-----------------------------------------------------------
#removing EducationOthers
logistic_7 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + ProfessionSE + 
                    Type.of.residenceOthers 
                  , family = "binomial", data = final_data_set)

summary(logistic_7)
vif(logistic_7)

#-----------------------------------------------------------
#removing ProfessionSE
logistic_8 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + 
                    Type.of.residenceOthers 
                  , family = "binomial", data = final_data_set)

summary(logistic_8)
vif(logistic_8)
#-----------------------------------------------------------
#removing Type.of.residenceOthers

logistic_9 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company 
                  , family = "binomial", data = final_data_set)

summary(logistic_9)
vif(logistic_9)

#-----------------------------------------------------------
#removing No.of.months.in.current.residence

logistic_10 <- glm(Performance.Tag ~ Income + 
                    No.of.months.in.current.company 
                  , family = "binomial", data = final_data_set)

summary(logistic_10)
vif(logistic_10)

#-----------------------------------------------------------
logistic_final <- logistic_10
#-----------------------------------------------------------

#Getting column number for the performance tag column
coloumnNum <- which( colnames(test)=="Performance.Tag" )
coloumnNum

#Predicting on test data
predictions_logit <- predict(logistic_final, , newdata = test[, -coloumnNum], type = "response")
summary(predictions_logit)

#checking confusion matrix on 0.5 as the cutoff
predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

unique(predicted_response)
unique(as.factor(test$Performance.Tag))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, as.factor(test$Performance.Tag), positive = "yes")

conf
#Confusion Matrix and Statistics

#Reference
#Prediction    no   yes
#no  15468   620
#yes  4516   262

#Accuracy : 0.7539         
#95% CI : (0.748, 0.7597)
#No Information Rate : 0.9577         
#P-Value [Acc > NIR] : 1              

#Kappa : 0.0228         
#Mcnemar's Test P-Value : <2e-16         
                                         
#            Sensitivity : 0.29705        
#            Specificity : 0.77402        
#         Pos Pred Value : 0.05483        
#         Neg Pred Value : 0.96146        
#            Prevalence : 0.04227        
#         Detection Rate : 0.01256        
#   Detection Prevalence : 0.22898        
#      Balanced Accuracy : 0.53554        
                                         
#       'Positive' Class : yes

#---------------------------------------------------------    
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


# Let's choose a cutoff value of 0.4 for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.46, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, as.factor(test$Performance.Tag), positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#0.5718394 
sens
#0.5124717 

spec
#0.5744596 

#The logistic model created from demographic data is not much better than a random model
#More data is required

#-----------------------------------------------------------
#Building a Random Forest on Demographic dataset
#-----------------------------------------------------------

#random forest


# Building the model 

credit_rf <- randomForest(Performance.Tag ~., data = final_data_set, proximity = F, do.trace = T, mtry = 3)
credit_rf$importance

# Gini Indexes
#MeanDecreaseGini
#Age                                        860.613939
#No.of.dependents                           372.154827
#Income                                     922.883588
#No.of.months.in.current.residence          804.989192
#No.of.months.in.current.company            965.033837
#gender_dummy                               133.786992
#marital_status_dummy                       110.790892
#EducationMasters                           114.905465
#EducationOthers                              7.480302
#EducationPhd                                68.835796
#EducationProfessional                      116.232013
#ProfessionSE                               106.113892
#ProfessionSE_PROF                          114.951108
#Type.of.residenceLiving.with.Parents        26.411760
#Type.of.residenceOthers                      7.341854
#Type.of.residenceOwned                      87.798324
#Type.of.residenceRented                     96.855311
# Predict response for test data

credit_rf_pred <- predict(credit_rf, test, type = "prob")


#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(credit_rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]
cutoff_rf

# The plot shows that cutoff value of around 0.29 optimises sensitivity and accuracy

predicted_response_22 <- factor(ifelse(credit_rf_pred[, 2] >= 0.29, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_22, as.factor(test$Performance.Tag), positive = "yes")

conf_forest


#Confusion Matrix and Statistics

#Reference
#Prediction    no   yes
#no  10589   370
#yes  9395   512

#Accuracy : 0.532           
#95% CI : (0.5252, 0.5388)
#No Information Rate : 0.9577          
#P-Value [Acc > NIR] : 1               

#Kappa : 0.0187          
#Mcnemar's Test P-Value : <2e-16          

#Sensitivity : 0.58050         
#Specificity : 0.52987         
#Pos Pred Value : 0.05168         
#Neg Pred Value : 0.96624         
#Prevalence : 0.04227         
#Detection Rate : 0.02454         
#Detection Prevalence : 0.47479         
#Balanced Accuracy : 0.55519         

#'Positive' Class : yes

# The accuracy sensitivity and specificity are not much better than a random model.
# Need more data to create a better model


#---------------------------------------------------------    
#Adding credit beuraeu data to the demographic data to create a better model
#---------------------------------------------------------    

#Credit bureau data


View(credit_data_matched)
nrow(credit_data_matched) #69551 rows
ncol(credit_data_matched) #19 columns
head(credit_data_matched)
tail(credit_data_matched)
str(credit_data_matched)
summary(credit_data_matched)

# Checking missing values
sum(is.na(credit_data_matched)) # 1558 blanks

#Check duplicate rows

length(unique(tolower(credit_data_matched$Application.ID))) 

dup_cred <-  duplicated(credit_data_matched$Application.ID) 
sum(dup_cred)
#No duplictes found

#Check application id for the duplicate records
dup_cred_id <- credit_data_matched[duplicated(credit_data_matched$Application.ID),]$Application.ID

#View duplicate application ids (0)
dup_cred_id



#rename long names with short name for ease of coding
#********************************************************************#
#********************************************************************#

#get column names - credit 
colnames(credit_data_matched)

credit_data_matched = credit_data_matched %>% rename(
  
  No_of_90dpd_L6M = No.of.times.90.DPD.or.worse.in.last.6.months,
  No_of_60dpd_L6M = No.of.times.60.DPD.or.worse.in.last.6.months, 
  No_of_30dpd_L6M = No.of.times.30.DPD.or.worse.in.last.6.months,
  
  No_of_90dpd_L12M = No.of.times.90.DPD.or.worse.in.last.12.months,
  No_of_60dpd_L12M = No.of.times.60.DPD.or.worse.in.last.12.months, 
  No_of_30dpd_L12M = No.of.times.30.DPD.or.worse.in.last.12.months,
  
  Avg_CC_Util_L12M = Avgas.CC.Utilization.in.last.12.months,
  No_of_trades_opnd_L6M = No.of.trades.opened.in.last.6.months,
  No_of_trades_opnd_L12M = No.of.trades.opened.in.last.12.months,
  
  No_of_PL_trades_opnd_L6M = No.of.PL.trades.opened.in.last.6.months,
  No_of_PL_trades_opnd_L12M = No.of.PL.trades.opened.in.last.12.months,
  
  No_of_Inq_ex_HLAL_L6M = No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,
  No_of_Inq_ex_HLAL_L12M = No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,
  
  Presence_of_opn_HL = Presence.of.open.home.loan,
  Outstanding_Bal =  Outstanding.Balance,
  
  Total_No_of_Trades = Total.No.of.Trades,
  Presence_of_open_AL  = Presence.of.open.auto.loan,                                         
  Performance_Tag = Performance.Tag
)

colnames(credit_data_matched)

#********************************************************************#
#********************************************************************#                                                


summary(credit_data_matched) # many na values are there need to check variable wise

#********************************************************************#
#********************************************************************#
#Storing the Details of data having performance tag as NA

#Check NA for perf variable
perf_na=sum(is.na(credit_data_matched$Performance_Tag)) 
perf_na #0 


#********************************************************************#
#********************************************************************#

#Finding columns with NA values
colnames(credit_data_matched)[colSums(is.na(credit_data_matched)) > 0]
#"Avg_CC_Util_L12M"      "No_of_trades_opnd_L6M" "Presence_of_opn_HL"    "Outstanding_Bal"
data.frame(colSums(is.na(credit_data_matched)))
colSums.is.na.credit_data_matched
#App_ID                                              0
#No_of_90dpd_L6M                                     0
#No_of_60dpd_L6M                                     0
#No_of_30dpd_L6M                                     0
#No_of_90dpd_L12M                                    0
#No_of_60dpd_L12M                                    0
#No_of_30dpd_L12M                                    0
#Avg_CC_Util_L12M                                    0
#No_of_trades_opnd_L6M                               1015
#No_of_trades_opnd_L12M                              1
#No_of_PL_trades_opnd_L6M                            0
#No_of_PL_trades_opnd_L12M                           0
#No_of_Inq_ex_HLAL_L6M                               0
#No_of_Inq_ex_HLAL_L12M                              0
#Presence_of_opn_HL                                  271
#Outstanding_Bal                                     271
#Total_No_of_Trades                                  0
#Presence_of_open_AL                                 0
#Performance_Tag                                     0

###################################################
# Variable wise analysis for all char variable- Univariate Analysis
##################################################

#********************************************************************#

#No_of_90dpd_L6M 

table(credit_data_matched$No_of_90dpd_L6M) 
#0     1     2     3 
#54394 13183  1771   206 

summary(credit_data_matched$No_of_90dpd_L6M) 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.2493  0.0000  3.0000

histogram(credit_data_matched$No_of_90dpd_L6M) 


#********************************************************************#

table(credit_data_matched$No_of_60dpd_L6M) 
#0     1     2     3     4     5 
#51608 11101  4902  1466   407    70  

#No_of_60dpd_L6M 
summary(credit_data_matched$No_of_60dpd_L6M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.3922  1.0000  5.0000 

histogram(credit_data_matched$No_of_60dpd_L6M) 

#********************************************************************#

#No_of_30dpd_L6M 
table(credit_data_matched$No_of_30dpd_L6M)  
#0     1     2     3     4     5     6     7 
#49840  9471  5886  2823  1042   381    96    15

summary(credit_data_matched$No_of_30dpd_L6M)   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.5243  1.0000  7.0000

histogram(credit_data_matched$No_of_30dpd_L6M) 

#********************************************************************#

#No_of_90dpd_L12M 
table(credit_data_matched$No_of_90dpd_L12M)  
#0     1     2     3     4     5 
#50231 11634  6144  1242   267    36 

summary(credit_data_matched$No_of_90dpd_L12M) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.4154  1.0000  5.0000 

histogram(credit_data_matched$No_of_90dpd_L12M) 
#********************************************************************#

#No_of_60dpd_L12M 
table(credit_data_matched$No_of_60dpd_L12M)  
#0     1     2     3     4     5     6     7 
#45627 12771  6407  3194  1043   394   111     7 

summary(credit_data_matched$No_of_60dpd_L12M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.6042  1.0000  7.0000 

histogram(credit_data_matched$No_of_60dpd_L12M) 
#********************************************************************#

#No_of_30dpd_L12M 
table(credit_data_matched$No_of_30dpd_L12M)  
#0     1     2     3     4     5     6     7     8     9 
#44619 11433  6101  4130  1918   849   373   107    23     1

summary(credit_data_matched$No_of_30dpd_L12M)   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   0.735   1.000   9.000 

histogram(credit_data_matched$No_of_30dpd_L12M) 
boxplot(credit_data_matched$No_of_30dpd_L12M,ylab = "No_of_30dpd_L12M",col = "blue")
#********************************************************************#

#No_of_trades_opnd_L6M 

summary(credit_data_matched$No_of_trades_opnd_L6M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs
#0.000   1.000   2.000   2.289   3.000  12.000       1

histogram(credit_data_matched$No_of_trades_opnd_L6M) 
boxplot(credit_data_matched$No_of_trades_opnd_L6M,ylab = "No_of_trades_opnd_L6M",col = "blue")
# most users have 0-4 trades opened in last 6 mon.
# Outlier do exist.

#********************************************************************#

#No_of_trades_opnd_L12M 
summary(credit_data_matched$No_of_trades_opnd_L12M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   2.000   4.000   5.795   9.000  28.000 

histogram(credit_data_matched$No_of_trades_opnd_L12M) 
boxplot(credit_data_matched$No_of_trades_opnd_L12M,ylab = "No_of_trades_opnd_L12M",col = "blue")
# most users have 0-10 trades opened in last 12 mon.
# Outlier do exist.

#********************************************************************#

#No_of_PL_trades_opnd_L6M  
summary(credit_data_matched$No_of_PL_trades_opnd_L6M )   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   1.000   1.192   2.000   6.000  


histogram(credit_data_matched$No_of_PL_trades_opnd_L6M) 
boxplot(credit_data_matched$No_of_PL_trades_opnd_L6M,ylab = "No_of_PL_trades_opnd_L6M",col = "blue")
# most users have 0-3 PL opened in last 6 mon.
# Very few Outlier are there.


#********************************************************************#

#No_of_PL_trades_opnd_L12M 
table(credit_data_matched$No_of_PL_trades_opnd_L12M)  
#0     1     2     3     4     5     6     7     8     9    10    11    12 
#25626  6624  6814  8107  7878  6176  4011  2219  1171   599   253    66    10 

summary(credit_data_matched$No_of_PL_trades_opnd_L12M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   2.000   2.368   4.000  12.000 

histogram(credit_data_matched$No_of_PL_trades_opnd_L12M) 
boxplot(credit_data_matched$No_of_PL_trades_opnd_L12M,ylab = "No_of_PL_trades_opnd_L12M",col = "blue")
# most users have 0-6 trades opened in last 12 mon.
# Outlier might be there.

#********************************************************************#

#No_of_Inq_ex_HLAL_L6M  
table(credit_data_matched$No_of_Inq_ex_HLAL_L6M )  
#0     1     2     3     4     5     6     7     8     9    10 
#24874 13134 12803  7234  4237  3015  1745  1145   834   425   108


summary(credit_data_matched$No_of_Inq_ex_HLAL_L6M ) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   1.000   1.762   3.000  10.000 

histogram(credit_data_matched$No_of_Inq_ex_HLAL_L6M) 
boxplot(credit_data_matched$No_of_Inq_ex_HLAL_L6M,ylab = "No_of_Inq_ex_HLAL_L6M",col = "blue")
#********************************************************************#

#Presence_of_opn_HL  
table(credit_data_matched$Presence_of_opn_HL )  
#0     1 
#51326 17957 

histogram(credit_data_matched$Presence_of_opn_HL) 
#********************************************************************#

#Presence_of_open_AL  
table(credit_data_matched$Presence_of_open_AL ) 
#0     1 
#63660  5894 

histogram(credit_data_matched$Presence_of_open_AL) 

#********************************************************************#

#Presence_of_open_AL  
table(credit_data_matched$Presence_of_open_AL )  
#0     1 
#63660  5894 
histogram(credit_data_matched$Presence_of_open_AL) 

#********************************************************************#

table(credit_data_matched$Performance_Tag) 
#0     1 
#66614  2940

histogram(credit_data_matched$Performance_Tag) 

#********************************************************************#

#No_of_Inq_ex_HLAL_L12M 
summary(credit_data_matched$No_of_Inq_ex_HLAL_L12M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   3.000   3.532   5.000  20.000  

histogram(credit_data_matched$No_of_Inq_ex_HLAL_L12M) 
boxplot(credit_data_matched$No_of_PL_trades_opnd_L12M,ylab = "No_of_Inq_ex_HLAL_L12M",col = "blue")

#checks quantiles
quantile(credit_data_matched$No_of_Inq_ex_HLAL_L12M,seq(0,1,0.001))# not sure if we need to cap this variable

#********************************************************************#
#Avg_CC_Util_L12M   
summary(credit_data_matched$Avg_CC_Util_L12M  )   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00    8.00   15.00   29.29   45.00  113.00    1015 

histogram(credit_data_matched$Avg_CC_Util_L12M) 
boxplot(credit_data_matched$Avg_CC_Util_L12M,ylab = "Avg_CC_Util_L12M",col = "blue")
## most users are utilizing only upto 20% of card upper limit, 
## population size with proper 25 to 60 % card utilization is similar
## Left skewed ..outliers do exist.

#checks quantiles
quantile(credit_data_matched$Avg_CC_Util_L12M,seq(0,1,0.001), na.rm = TRUE) #not sure if we need to cap this variable


#********************************************************************#
#Total_No_of_Trades   
summary(credit_data_matched$Total_No_of_Trades)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   3.000   6.000   8.188  10.000  44.000 

histogram(credit_data_matched$Total_No_of_Trades) 
boxplot(credit_data_matched$Total_No_of_Trades,ylab = "Total_No_of_Trades",col = "blue")

# most users have 0-10 trades in total
# Outlier are there.


#checks quantiles
quantile(credit_data_matched$Total_No_of_Trades,seq(0,1,0.001)) 

#********************************************************************#

#Outstanding_Bal 
summary(credit_data_matched$Outstanding_Bal)   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0  208664  774228 1252765 2925358 5218801     271

#checks quantiles
quantile(credit_data_matched$Outstanding_Bal,seq(0,1,0.001), na.rm = TRUE) #income is 4.5 for first 5%, hence taking benchmark

boxplot(credit_data_matched$Outstanding_Bal,ylab = "Outstanding_Bal",col = "blue")
# 0-200000 range higher no of users
# 300k upwards lower no of users


#********************************************************************#

#Ploting the histogram of No_of_90dpd_L6M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_90dpd_L6M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins=20)+
  labs(title="Distribution of No_of_90dpd_L6M vs Performance_Tag",x="90dpd_L6M",y="Count",fill="target")  


#Ploting the histogram of No_of_60dpd_L6M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_60dpd_L6M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_60dpd_L6M vs Performance_Tag",x="60dpd_L6M",y="Count",fill="target") 


#No_of_30dpd_L6M
#Ploting the histogram of No_of_30dpd_L6M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_30dpd_L6M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_30dpd_L6M vs Performance_Tag",x="30dpd_L6M",y="Count",fill="target") 

#customer moving from 60 to 90
#Ploting the histogram of No_of_60dpd_L6M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_90dpd_L6M,fill=factor(credit_data_matched$No_of_60dpd_L6M)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_90dpd_L6M vs No_of_60dpd_L6M",x="90dpd_L6M",y="Count",fill="No_of_60dpd_L6M")  

#********************************************************************#
#********************************************************************#

#Ploting the histogram of No_of_90dpd_L12M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_90dpd_L12M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_90dpd_L12M vs Performance_Tag",x="90dpd_L12M",y="Count",fill="target")  


#Ploting the histogram of No_of_60dpd_L12M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_60dpd_L12M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_60dpd_L12M vs Performance_Tag",x="60dpd_L12M",y="Count",fill="target")  


#No_of_30dpd_L6M
#Ploting the histogram of No_of_30dpd_L6M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_30dpd_L12M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_30dpd_L12M vs Performance_Tag",x="30dpd_L12M",y="Count",fill="target") 

#********************************************************************#

#Ploting the histogram of No_of_90dpd_L12M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_90dpd_L12M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_90dpd_L12M vs Performance_Tag",x="90dpd_L12M",y="Count",fill="target")  


#Ploting the histogram of No_of_60dpd_L12M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_60dpd_L12M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_60dpd_L12M vs Performance_Tag",x="60dpd_L12M",y="Count",fill="target")  


#No_of_30dpd_L6M
#Ploting the histogram of No_of_30dpd_L6M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_30dpd_L12M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_30dpd_L12M vs Performance_Tag",x="30dpd_L12M",y="Count",fill="target") 

#********************************************************************#

#Ploting the histogram of No_of_trades_opnd_L6M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_trades_opnd_L6M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_trades_opnd_L6M vs Performance_Tag",x="No_of_trades_opnd_L6M",y="Count",fill="target")  


#Ploting the histogram of No_of_trades_opnd_L12M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_trades_opnd_L12M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_trades_opnd_L12M vs Performance_Tag",x="trades_opnd_L12M",y="Count",fill="target")  


#No_of_trades_opnd_L6M vs No_of_trades_opnd_L12M
ggplot(data=credit_data_matched,aes(x=No_of_trades_opnd_L12M,fill=factor(credit_data_matched$No_of_trades_opnd_L6M)))+geom_histogram(bins = 10)+
  labs(title="Distribution of No_of_trades_opnd_L12M vs No_of_trades_opnd_L6M",x="30dpd_L12M",y="Count",fill="target") 

#********************************************************************#

#Ploting the histogram of No_of_PL_trades_opnd_L6M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_PL_trades_opnd_L6M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_PL_trades_opnd_L6M vs Performance_Tag",x="No_of_PL_trades_opnd_L6M",y="Count",fill="target")  


#Ploting the histogram of No_of_trades_opnd_L12M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_PL_trades_opnd_L12M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_PL_trades_opnd_L12M vs Performance_Tag",x="No_of_PL_trades_opnd_L12M",y="Count",fill="target")  


#********************************************************************#

#Ploting the histogram of No_of_Inq_ex_HLAL_L6M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_Inq_ex_HLAL_L6M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 25)+
  labs(title="Distribution of No_of_Inq_ex_HLAL_L6M vs Performance_Tag",x="No_of_Inq_ex_HLAL_L6M",y="Count",fill="target")  


#Ploting the histogram of No_of_Inq_ex_HLAL_L12M vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=No_of_Inq_ex_HLAL_L12M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_Inq_ex_HLAL_L12M vs Performance_Tag",x="No_of_Inq_ex_HLAL_L12M",y="Count",fill="target")  


#********************************************************************#

#Ploting the histogram of Presence_of_opn_HL vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=Presence_of_opn_HL,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of Presence_of_opn_HL vs Performance_Tag",x="Presence_of_opn_HL",y="Count",fill="target")  

#Ploting the histogram of Presence_of_open_AL vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=Presence_of_open_AL,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of Presence_of_open_AL vs Performance_Tag",x="Presence_of_open_AL",y="Count",fill="target")  


#Ploting the histogram of Total_No_of_Trades vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=Total_No_of_Trades,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of Total_No_of_Trades vs Performance_Tag",x="Total_No_of_Trades",y="Count",fill="target")  

#********************************************************************#

#Ploting the histogram of Presence_of_opn_HL vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=Presence_of_opn_HL,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of Outstanding_Bal vs Performance_Tag",x="Outstanding_Bal",y="Count",fill="target")  

#Ploting the histogram of Presence_of_open_AL vs Performance_Tag
ggplot(data=credit_data_matched,aes(x=Avg_CC_Util_L12M,fill=factor(credit_data_matched$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of Avg_CC_Util_L12M vs Performance_Tag",x="Avg_CC_Util_L12M",y="Count",fill="target")  


#********************************************************************#
##Merging credit beureu data and demographic data based on application id

merged_dataframe <- merge(x=demog_data_with_dummy,y=credit_data_matched,by="Application.ID")
str(merged_dataframe)
nrow(merged_dataframe)

#checking if both the performance tag values are same
identical(merged_dataframe$Performance.Tag, merged_dataframe$Performance_Tag)
#TRUE - both performance tag values are identical

#removing a performance tag variable as its redundant
merged_dataframe <- merged_dataframe[ , !(names(merged_dataframe) %in% c("Performance.Tag"))]

#********************************************************************#
# Converting all variables to woe for further analysis
#********************************************************************#

binning <- woe.binning(merged_dataframe, "Performance_Tag", colnames(merged_dataframe))


#plotting the woe
woe.binning.plot(binning)


#adding woe values in the data frame
merged_data_all_woe<- woe.binning.deploy(merged_dataframe, binning, add.woe.or.dum.var='woe')

#saving the merged file with all woe values
write.csv(merged_data_all_woe, file = "merged_data_all_woe.csv")

#********************************************************************#
# Handling NA values in the dataset
#********************************************************************#

#binning based on woe and IV for columns with NA values
str(merged_dataframe)
binning <- woe.binning(merged_dataframe, "Performance_Tag", c("No_of_trades_opnd_L6M", "Presence_of_opn_HL", "Outstanding_Bal",
                                                                 "Avg_CC_Util_L12M"))

binning[1,]
#Avg_CC_Util_L12M

#                woe cutpoints.final cutpoints.final[-1] iv.total.final    1     0 col.perc.a col.perc.b      iv.bins
#(-Inf,12] -73.89592            -Inf                  12      0.2486099  616 29223 0.20952381 0.43869157 0.1693456226
#(12, Inf]  34.76968              12                 Inf      0.2486099 2276 36424 0.77414966 0.54679197 0.0790515433
#Missing    11.75077             Inf             Missing      0.2486099   48   967 0.01632653 0.01451647 0.0002126963


#iv.total.final 
#0.2486099 

binning[2,]
#No_of_trades_opnd_L6M

#               woe cutpoints.final cutpoints.final[-1] iv.total.final    1     0   col.perc.a   col.perc.b      iv.bins
#(-Inf,1] -54.21047            -Inf                   1      0.1685564  803 31292 2.731473e-01 0.4697101903 1.065577e-01
#(1, Inf]  31.53795               1                 Inf      0.1685564 2137 35321 7.267527e-01 0.5301748323 6.199663e-02
#Missing  -13.98651             Inf             Missing      0.1685564    0     1 9.997001e-05 0.0001149774 2.099005e-06


#iv.total.final 
#0.1685564

binning[3,]
#Presence_of_opn_HL

#                woe cutpoints.final cutpoints.final[-1] iv.total.final    1     0  col.perc.a  col.perc.b      iv.bins
#(-Inf,0]   7.283494            -Inf                   0     0.01722315 2326 49000 0.791156463 0.735581109 0.0040478276
#(0, Inf] -23.402025               0                 Inf     0.01722315  606 17351 0.206122449 0.260470772 0.0127186081
#Missing  -37.220731             Inf             Missing     0.01722315    8   263 0.002721088 0.003948119 0.0004567097


#iv.total.final 
#0.01722315 

binning[4,]
#Outstanding_Bal

#                     woe cutpoints.final cutpoints.final[-1] iv.total.final    1     0  col.perc.a  col.perc.b      iv.bins
#(-Inf,2661.1] -59.929374            -Inf              2661.1     0.01478471   82  3383 0.027891156 0.050785120 0.0137202091
#(2661.1, Inf]   2.519752          2661.1                 Inf     0.01478471 2850 62968 0.969387755 0.945266761 0.0006077892
#Missing       -37.220731             Inf             Missing     0.01478471    8   263 0.002721088 0.003948119 0.0004567097


#iv.total.final 
#0.01478471



#plotting the woe

woe.binning.plot(binning)


#adding woe values in the data frame
merged_dataframe <- woe.binning.deploy(merged_dataframe, binning, add.woe.or.dum.var='woe')

#********************************************************************#

#removing variables repalced by woe from the data frame
str(merged_dataframe)

colums_to_be_dropped <- c("Avg_CC_Util_L12M","No_of_trades_opnd_L6M","Presence_of_opn_HL","Outstanding_Bal" ,"Avg_CC_Util_L12M.binned",
                          "No_of_trades_opnd_L6M.binned", "Presence_of_opn_HL.binned", "Outstanding_Bal.binned")

merged_dataframe <- merged_dataframe[ , !(names(merged_dataframe) %in% colums_to_be_dropped)]



str(merged_dataframe)


#**************************************************************************************************#
#***** Getting Information Value for all variables *************************************************#
#**************************************************************************************************#

colnames(merged_dataframe)
information_value <- woe.binning( merged_dataframe, "Performance_Tag", colnames(merged_dataframe))

woe.binning.plot(information_value)
information_value

     
#[2,] "No_of_PL_trades_opnd_L12M"            List,9 0.2683828   
#[3,] "No_of_Inq_ex_HLAL_L12M"               List,9 0.2617236   
#[4,] "No_of_trades_opnd_L12M"               List,9 0.2530941   
#[5,] "woe.Avg_CC_Util_L12M.binned"          List,9 0.2476252   
#[6,] "No_of_30dpd_L6M"                      List,9 0.2344866   
#[7,] "No_of_30dpd_L12M"                     List,9 0.2139293   
#[8,] "No_of_PL_trades_opnd_L6M"             List,9 0.2121027   
#[9,] "No_of_90dpd_L12M"                     List,9 0.209882    
#[10,] "No_of_60dpd_L6M"                      List,9 0.2060749   
#[11,] "Total_No_of_Trades"                   List,9 0.1864533   
#[12,] "No_of_60dpd_L12M"                     List,9 0.1852066   
#[13,] "No_of_Inq_ex_HLAL_L6M"                List,9 0.1828772   
#[14,] "woe.No_of_trades_opnd_L6M.binned"     List,9 0.1684615   
#[15,] "No_of_90dpd_L6M"                      List,9 0.160066    
#[16,] "No.of.months.in.current.residence"    List,9 0.09256043  
#[17,] "Income"                               List,9 0.03644146  
#[18,] "No.of.months.in.current.company"      List,9 0.02663807  
#[19,] "woe.Presence_of_opn_HL.binned"        List,9 0.01711969  
#[20,] "woe.Outstanding_Bal.binned"           List,9 0.0145947   
#[21,] "Age"                                  List,9 0.003044048 
#[22,] "No.of.dependents"                     List,9 0.002502809 
#[23,] "ProfessionSE"                         List,9 0.002221965 
#[24,] "Presence_of_open_AL"                  List,9 0.001554384 
#[25,] "Application.ID"                       List,9 0.001211282 
#[26,] "Type.of.residenceOthers"              List,9 0.0006241263
#[27,] "EducationOthers"                      List,9 0.0005536898
#[28,] "gender_dummy"                         List,9 0.0003414735
#[29,] "Type.of.residenceLiving.with.Parents" List,9 0.0001639274
#[30,] "EducationProfessional"                List,9 0.0001564454
#[31,] "marital_status_dummy"                 List,9 0.0001093655
#[32,] "ProfessionSE_PROF"                    List,9 7.530029e-05
#[33,] "EducationPhd"                         List,9 5.514656e-05
#[34,] "Type.of.residenceRented"              List,9 4.805032e-05
#[35,] "EducationMasters"                     List,9 3.732545e-05
#[36,] "Type.of.residenceOwned"               List,9 1.650513e-06

#********************************************************************#
# Modelling on Merged Data set of demographic Data and credit beureau data for predicting Perforamce Tag
#********************************************************************#

#Finding distribution of Perforance Tag values oin the dataset

nrow(merged_dataframe[which(merged_dataframe$Performance_Tag == 1),]) /
  nrow(merged_dataframe)
# 0.04225676 - around 4 percent of the dataset is default case, The data set is skewed




#Splitting data between test and train
#splitting data into train and test
set.seed(30)

split_indices <- sample.split(merged_dataframe$Performance_Tag, SplitRatio = 0.70)

train <- merged_dataframe[split_indices, ]

test <- merged_dataframe[!split_indices, ]

nrow(train)/nrow(merged_dataframe)

nrow(test)/nrow(merged_dataframe)

table(train$Performance_Tag) 
table(test$Performance_Tag) 


#**************************************************************************************************#
#***** Since Performance tag is very skewed. Taking measures to avoid bias ************************#
#**************************************************************************************************#
#segregating non-default datapoint into appropriate number of clusters and pick 2000 points 
#from each of the clusters to downscale the amount of non-default data-points using stratified sampling
#and replicate the default cases to increase the amount of default cases in the dataset

# segregating default and non default cases
merged_dataframe_Default <- train[which(train$Performance_Tag == 1),]
merged_dataframe_nonDefault <- train[which(train$Performance_Tag == 0),]


#Confirming no NAs before clustering
colnames(merged_dataframe_nonDefault)[colSums(is.na(merged_dataframe_nonDefault)) > 0]
colSums(is.na(merged_dataframe_nonDefault))
str(merged_dataframe_nonDefault)
r_sq<- rnorm(20)


#Cheking for the right number of clusters possible on non-default data set
for (number in 1:20){
  clus <- kmeans(merged_dataframe_nonDefault[,-1], centers = number, nstart = 50)
  r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)

# 5 clusters seems to be a decent cluster size
## Running the K-Means algorithm for K =5

#Clustering non-default data set into 5 clusters

clus5 <- kmeans(merged_dataframe_nonDefault[,-1], centers = 5, iter.max = 50, nstart = 50)

## Appending the ClusterIDs to RFM data

non_default_data <-cbind(merged_dataframe_nonDefault,clus5$cluster)

colnames(non_default_data)[37] <- "ClusterID"

#taking out 2000 random data points from each cluster formed on non-default data set
cluster1 <- non_default_data[which(non_default_data$ClusterID == "1"),]
cluster2 <- non_default_data[which(non_default_data$ClusterID == "2"),]
cluster3 <- non_default_data[which(non_default_data$ClusterID == "3"),]
cluster4 <- non_default_data[which(non_default_data$ClusterID == "4"),]
cluster5 <- non_default_data[which(non_default_data$ClusterID == "5"),]

downscale_non_default <- rbind(cluster1[sample(nrow(cluster1), 2000), ],
                               cluster2[sample(nrow(cluster2), 2000), ],
                               cluster3[sample(nrow(cluster3), 2000), ],
                               cluster4[sample(nrow(cluster4), 2000), ],
                               cluster5[sample(nrow(cluster5), 2000), ])

#combining the deafulat data set with the randomly selected 10000 non-default data sets
#replicating the default cases to increase the amount of default data-points in the data-set
final_data_set <- rbind(downscale_non_default[,-37], merged_dataframe_Default)
#final_data_set <- rbind(final_data_set, merged_dataframe_Default)
#final_data_set <- rbind(final_data_set, merged_dataframe_Default)
#final_data_set <- rbind(final_data_set, merged_dataframe_Default)
#final_data_set <- rbind(final_data_set, merged_dataframe_Default)

#After treatment, checking the distribution of data set
nrow(final_data_set[which(final_data_set$Performance_Tag == 1),]) /
  nrow(final_data_set)
# 0.1706063 - around 17% of the data set is default case

# removing Application.ID as it is irrelevant to the Model

final_data_set <- final_data_set[ , !(names(final_data_set) %in% c("Application.ID"))]


#changing Preformance tag to factor
final_data_set$Performance_Tag <- as.factor(ifelse(final_data_set$Performance_Tag == 1, "yes", "no"))
test$Performance_Tag <- as.factor(ifelse(test$Performance_Tag == 1, "yes", "no"))


#********************************************************************#
#Training a Logistic model on Merged data set
#********************************************************************#

logistic_1 <- glm(Performance_Tag ~ ., family = "binomial", data = final_data_set)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")

summary(logistic_2)

#-----------------------------------------------------------
# stepAIC has removed some variables and only the following ones remain

logistic_3 <- glm(Performance_Tag ~ Age + No.of.months.in.current.residence + No.of.months.in.current.company + 
                    gender_dummy + marital_status_dummy + EducationOthers + ProfessionSE + 
                    No_of_90dpd_L6M + No_of_30dpd_L6M + No_of_90dpd_L12M + No_of_PL_trades_opnd_L6M + 
                    No_of_PL_trades_opnd_L12M + No_of_Inq_ex_HLAL_L6M + No_of_Inq_ex_HLAL_L12M + 
                    Total_No_of_Trades + woe.Avg_CC_Util_L12M.binned + woe.No_of_trades_opnd_L6M.binned, family = "binomial", data = final_data_set)

summary(logistic_3)
vif(logistic_3)

#-----------------------------------------------------------
#removing No_of_PL_trades_opnd_L12M

logistic_4 <- glm(Performance_Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + EducationProfessional + 
                    No_of_60dpd_L6M + No_of_30dpd_L6M + No_of_90dpd_L12M + No_of_PL_trades_opnd_L6M + 
                    No_of_Inq_ex_HLAL_L6M + No_of_Inq_ex_HLAL_L12M + 
                    Total_No_of_Trades + woe.Avg_CC_Util_L12M.binned + woe.No_of_trades_opnd_L6M.binned + 
                    woe.Outstanding_Bal.binned, family = "binomial", data = final_data_set)

vif(logistic_4)

summary(logistic_4)

#-----------------------------------------------------------
#removing No_of_Inq_ex_HLAL_L12M

logistic_5 <- glm(Performance_Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + EducationProfessional + 
                    No_of_60dpd_L6M + No_of_30dpd_L6M + No_of_90dpd_L12M + No_of_PL_trades_opnd_L6M + 
                    No_of_Inq_ex_HLAL_L6M + 
                    Total_No_of_Trades + woe.Avg_CC_Util_L12M.binned + woe.No_of_trades_opnd_L6M.binned + 
                    woe.Outstanding_Bal.binned, family = "binomial", data = final_data_set)

vif(logistic_5)

summary(logistic_5)

#-----------------------------------------------------------
#removing No_of_Inq_ex_HLAL_L6M

logistic_6 <- glm(Performance_Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + EducationProfessional + 
                    No_of_60dpd_L6M + No_of_30dpd_L6M + No_of_90dpd_L12M + No_of_PL_trades_opnd_L6M + 
                    Total_No_of_Trades + woe.Avg_CC_Util_L12M.binned + woe.No_of_trades_opnd_L6M.binned + 
                    woe.Outstanding_Bal.binned, family = "binomial", data = final_data_set)

vif(logistic_6)

summary(logistic_6)

#-----------------------------------------------------------
#removing woe.Outstanding_Bal.binned

logistic_7 <- glm(Performance_Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + EducationProfessional + 
                    No_of_60dpd_L6M + No_of_30dpd_L6M + No_of_90dpd_L12M + No_of_PL_trades_opnd_L6M + 
                    Total_No_of_Trades + woe.Avg_CC_Util_L12M.binned + woe.No_of_trades_opnd_L6M.binned
                  , family = "binomial", data = final_data_set)

vif(logistic_7)

summary(logistic_7)

#-----------------------------------------------------------
#removing Income

logistic_8 <- glm(Performance_Tag ~ No.of.months.in.current.residence + 
                    No.of.months.in.current.company + EducationProfessional + 
                    No_of_60dpd_L6M + No_of_30dpd_L6M + No_of_90dpd_L12M + No_of_PL_trades_opnd_L6M + 
                    Total_No_of_Trades + woe.Avg_CC_Util_L12M.binned + woe.No_of_trades_opnd_L6M.binned
                  , family = "binomial", data = final_data_set)

vif(logistic_8)

summary(logistic_8)


#-----------------------------------------------------------
#removing EducationProfessional

logistic_9 <- glm(Performance_Tag ~ No.of.months.in.current.residence + 
                    No.of.months.in.current.company + 
                    No_of_60dpd_L6M + No_of_30dpd_L6M + No_of_90dpd_L12M + No_of_PL_trades_opnd_L6M + 
                    Total_No_of_Trades + woe.Avg_CC_Util_L12M.binned + woe.No_of_trades_opnd_L6M.binned
                  , family = "binomial", data = final_data_set)

vif(logistic_9)

summary(logistic_9)

#-----------------------------------------------------------
#removing No_of_60dpd_L6M

logistic_10 <- glm(Performance_Tag ~ No.of.months.in.current.residence + 
                     No.of.months.in.current.company + 
                     No_of_30dpd_L6M + No_of_90dpd_L12M + No_of_PL_trades_opnd_L6M + 
                     Total_No_of_Trades + woe.Avg_CC_Util_L12M.binned + woe.No_of_trades_opnd_L6M.binned
                   , family = "binomial", data = final_data_set)

vif(logistic_10)

summary(logistic_10)

#-----------------------------------------------------------


#-----------------------------------------------------------
logistic_final <- logistic_10
#-----------------------------------------------------------
#Evaluating the final Logistic model

#getting the column number of Performance_Tag column
coloumnNum <- which( colnames(test)=="Performance_Tag" )
coloumnNum
predictions_logit <- predict(logistic_final, , newdata = test[, -coloumnNum], type = "response")
summary(predictions_logit)

#Checking the confusion matrix at cutoff 0.5
predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

unique(predicted_response)


# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, as.factor(test$Performance_Tag), positive = "yes")

conf

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$Performance_Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))



# Let's choose a cutoff value of 0.29 for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.28, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, as.factor(test$Performance_Tag), positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#Accuracy 
#0.629493 

#Sensitivity 
#0.6213152 

#Specificity 
#0.6298539 

# A better result than the result obtained from only the demographic data

#-----------------------------------------------------------
#Building a Random Forest on Merged dataset
#-----------------------------------------------------------

#random forest

merged_rf <- randomForest(Performance_Tag ~ ., data = final_data_set, proximity = F, do.trace = T, mtry = 3)
merged_rf$importance

#                                        MeanDecreaseGini
#Age                                        194.843847
#No.of.dependents                           107.313134
#Income                                     196.709046
#No.of.months.in.current.residence          181.550237
#No.of.months.in.current.company            204.518448
#gender_dummy                                39.088044
#marital_status_dummy                        31.024045
#EducationMasters                            38.837283
#EducationOthers                              1.871799
#EducationPhd                                20.009564
#EducationProfessional                       39.684090
#ProfessionSE                                34.898845
#ProfessionSE_PROF                           37.275132
#Type.of.residenceLiving.with.Parents        10.807288
#Type.of.residenceOthers                      1.358391
#Type.of.residenceOwned                      28.402394
#Type.of.residenceRented                     31.489244
#No_of_90dpd_L6M                             35.763641
#No_of_60dpd_L6M                             58.114652
#No_of_30dpd_L6M                             79.335464
#No_of_90dpd_L12M                            73.536375
#No_of_60dpd_L12M                            66.009791
#No_of_30dpd_L12M                            83.236683
#No_of_trades_opnd_L12M                     158.541190
#No_of_PL_trades_opnd_L6M                    92.581270
#No_of_PL_trades_opnd_L12M                  129.964713
#No_of_Inq_ex_HLAL_L6M                      111.884879
#No_of_Inq_ex_HLAL_L12M                     145.407957
#Total_No_of_Trades                         162.516551
#Presence_of_open_AL                         22.707030
#woe.Avg_CC_Util_L12M.binned                121.024729
#woe.No_of_trades_opnd_L6M.binned            53.863442
#woe.Presence_of_opn_HL.binned               35.795371
#woe.Outstanding_Bal.binned                   6.080367

## Predict response for test data

merged_rf_pred <- predict(merged_rf, test, type = "prob")

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(merged_rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$Performance_Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]
cutoff_rf

# The plot shows that cutoff value of around 0.2178788 optimises sensitivity and accuracy

predicted_response <- factor(ifelse(merged_rf_pred[, 2] >= 0.15, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response, as.factor(test$Performance_Tag), positive = "yes")
conf_forest
acc <- conf_forest$overall[1]

sens <- conf_forest$byClass[1]

spec <- conf_forest$byClass[2]

acc
#Accuracy : 0.5418863 

sens
#Sensitivity : 0.744898 

spec
#Specificity : 0.5329263


#=========================================
#Model Performance
#=========================================

#Adding the Predicted response to the test data-set
model_evaluation <- cbind(test, predicted_response)
dropped_users <-model_evaluation[which(model_evaluation$predicted_response == "yes"),]

#Total dropped users by the model
nrow(dropped_users)
#9991

#Percentage of dropped users
nrow(dropped_users) / nrow(model_evaluation)
#47.88172 %

nrow(dropped_users[which(dropped_users$Performance_Tag == "yes"),])
nrow(dropped_users[which(dropped_users$Performance_Tag == "no"),])

#Percentage of defaulting users rejected by the model(potential credit loss avoided)
nrow(dropped_users[which(dropped_users$Performance_Tag == "yes"),])/
  nrow(model_evaluation[which(model_evaluation$Performance_Tag == "yes"),])
# 74.5%

#Pecentage of non-defaulting users rejected by the model
nrow(dropped_users[which(dropped_users$Performance_Tag == "no"),])/
  nrow(model_evaluation[which(model_evaluation$Performance_Tag == "yes"),])   
#10.6%


#==============
#Comparing to a random model the Financial Gain(i.e 50% sensitive 50% accurate and 50% specific)
#==============

#Considering 100 applicants each appliying for a credit of 1000 INR
#Out of which 4 applcants would actually Default (Cosidering only 4% of data was for Default cases)
#96 applicants would Not-default

#A random model would reject 50% of the applicants that would not default
#There by loosing potential revenue

missed_revenue_random_model <- (96*(50/100)) * 1000
missed_revenue_random_model
#48000 INR

# The model suggested is rejecting 10.6% of the applicants that will not default
#There by loosing potential revenue 

missed_revenue_suggested_model <- (96*(10.6/100)) * 1000
missed_revenue_suggested_model
#10176 INR

#The suggested model increases potential revenue by
suggested_model_increase_revenue <- missed_revenue_random_model - missed_revenue_suggested_model
suggested_model_increase_revenue
#37824 INR


#A random model would accpet 50% of the applicants that would actually default
#There by incurring loss

loss_incurred_random_model <- (4*(50/100)) *1000
loss_incurred_random_model
#2000 INR

#The model suggested accepts 25.5 % of the applicants that would actuallty default
#Thereby incurring loss
loss_incurred_suggeseted_model <- (4*(25.5/100)) *1000
loss_incurred_suggeseted_model
#1020 INR

#The suggested model descreases loss by(Potential credit loss avoided)
suggested_model_reduced_loss <- loss_incurred_random_model - loss_incurred_suggeseted_model
suggested_model_reduced_loss
#980



#Total Financial gain by using the suggested model as compared to a random model
total_financial_gain <- suggested_model_reduced_loss + suggested_model_increase_revenue
total_financial_gain
#38804

#=============================
#Generating application score
#=============================

#Grabbing probability of good from prediction and calculating probability of good
probability_of_bad <- merged_rf_pred[, 2]
probability_of_good <- 1-probability_of_bad

#Log odds for the test data-set
log_odds = log(probability_of_good/ probability_of_bad)

#Building an application scorecard with the good to bad odds of 10 to 1 at a score of 400 doubling every 20 points. 
factor_for_score <- 20 / log(2)
factor_for_score
#28.8539

offset_for_score <- 400 - (factor_for_score * log(10))
offset_for_score
#333.5614

# Adding log odds and Application score values to the test data set
model_evaluation <- cbind(model_evaluation , log_odds)
model_evaluation$Application_Score <- offset_for_score + (factor_for_score * (model_evaluation$log_odds))
model_evaluation$Application_Score


#Score at odds 10

score_at_10 <- offset_for_score + (factor_for_score * log(10))
score_at_10
#400

#since score at odds 10 is 400, score at odds 20 should be 420
score_at_20 <- offset_for_score + (factor_for_score * log(20))
score_at_20
#420

#since score at odds 20 is 420, score at odds 40 should be 440
score_at_40 <- offset_for_score + (factor_for_score * log(40))
score_at_40
#440

#=================================
# calculating cutoff  Application score

#calculating application score 


calculate_application_score <- function(probability_of_bad)
{
  
  odds <- (1-probability_of_bad) / probability_of_bad
  return (offset_for_score + (factor_for_score * log(odds)))
}

#calculating application score at 0.000001 probabity of bad 0.15
cutoff_application_score <- calculate_application_score(0.15)
cutoff_application_score
#383.6114
#=================================
# The cutoff Application score is 383.6114
#=================================