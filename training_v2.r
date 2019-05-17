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
    "woeBinning"
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

View(demog_data)
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

View(credit_data)
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


#EDA on demographic data
#====================================

###################################################
# Variable wise analysis for all variable
##################################################

#********************************************************************#

#********************************************************************#

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




#-------------------------------------------------------

# Let's see the default rate of each age bucket in the plot
#Ploting the histogram of Age vs Performance_Tag
ggplot(agg_age, aes(age_range,default_rate ,label = No.of_IDs)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + labs(title="Default by Age bin",x="Age_range",y="Defaultrate",fill="#ofApps")

#Default rate is highest for the 55-65 age bracket, followed by 35-45 amd 25-35
#No of applicants is highest in the 35-45 bracket and lowest in 55-65 bracket

#********************************************************************#

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


#-------------------------------------------------------

# Let's see the default rate of each income bucket in the plot

#Ploting the histogram of Income vs Performance_Tag
ggplot(agg_income, aes(income_range,default_rate ,label = No.of_IDs)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + labs(title="Default by Income bin",x="income_range",y="Def_rate",fill="#ofApps")

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



#********************************************************************#

#********************************************************************#
#Binning categorical variables using WOE and IV


binning <- woe.binning(demog_data, "Performance.Tag", c("Gender", "Marital.Status..at.the.time.of.application.", "Education",
                                                        "Profession", "Type.of.residence"))


binning[1,]
#Profession

#  Group.2 Group.1       woe iv.total.final    1     0 col.perc.a col.perc.b      iv.bins
#1     SAL     SAL -2.792706    0.002303658 1626 37885  0.5530612  0.5687243 4.374234e-04
#3 SE_PROF SE_PROF -1.615122    0.002303658  673 15497  0.2289116  0.2326388 6.019916e-05
#2      SE      SE  9.314122    0.002303658  641 13232  0.2180272  0.1986369 1.806035e-03


#iv.total.final 
#0.002303658

binning[2,]
#Type of residence


#            Group.2             Group.1         woe iv.total.final    1     0 col.perc.a  col.perc.b      iv.bins
#3 misc. level neg.              Others -52.7552275   0.0008989849    5   192 0.00170068 0.002882277 6.233540e-04
#1   Owned + Rented               Owned  -0.2617675   0.0008989849 2783 63222 0.94659864 0.949079773 6.494801e-06
#2   Owned + Rented              Rented  -0.2617675   0.0008989849 2783 63222 0.94659864 0.949079773 6.494801e-06
#4 misc. level pos.    Company provided   7.3479616   0.0008989849  152  3200 0.05170068 0.048037950 2.691360e-04
#5 misc. level pos. Living with Parents   7.3479616   0.0008989849  152  3200 0.05170068 0.048037950 2.691360e-04


#iv.total.final 
#0.0008989849

binning[3,]
#Education

#           Group.2      Group.1       woe iv.total.final    1     0  col.perc.a  col.perc.b      iv.bins
#3 Professional + Phd          Phd -1.847868   0.0007633252 1194 27558 0.406122449 0.413696820 1.399644e-04
#4 Professional + Phd Professional -1.847868   0.0007633252 1194 27558 0.406122449 0.413696820 1.399644e-04
#1 Bachelor + Masters     Bachelor  1.103846   0.0007633252 1738 38947 0.591156463 0.584666887 7.163495e-05
#2 Bachelor + Masters      Masters  1.103846   0.0007633252 1738 38947 0.591156463 0.584666887 7.163495e-05
#5   misc. level pos.       Others 50.859884   0.0007633252    8   109 0.002721088 0.001636293 5.517259e-04


#iv.total.final 
#0.0007633252 


binning[4,]
#gender

#  Group.2 Group.1       woe iv.total.final    1     0 col.perc.a col.perc.b      iv.bins
#2       M       M -1.028397   0.0003349801 2223 50889  0.7561224  0.7639385 8.038016e-05
#1       F       F  3.257394   0.0003349801  717 15725  0.2438776  0.2360615 2.546000e-04


#iv.total.final 
#0.0003349801 


binning[5,]
#Martial status

# Group.2 Group.1        woe iv.total.final    1     0 col.perc.a col.perc.b      iv.bins
#1 Married Married -0.4324085   0.0001066424 2496 56799  0.8489796  0.8526586 1.590835e-05
#2  Single  Single  2.4662640   0.0001066424  444  9815  0.1510204  0.1473414 9.073406e-05


#iv.total.final 
#0.0001066424


#plotting the woe

woe.binning.plot(binning)


#adding woe values in the data frame
demog_data_all_woe<- woe.binning.deploy(demog_data, binning, add.woe.or.dum.var='woe')

#********************************************************************#

#removing variables repalced by woe from the data frame
str(demog_data_all_woe)

colums_to_be_dropped <- c("Gender", "Marital.Status..at.the.time.of.application.", "Education",
                          "Profession", "Type.of.residence", "Gender.binned", "Marital.Status..at.the.time.of.application..binned",
                          "Education.binned", "Profession.binned", "Type.of.residence.binned")

demog_data_all_woe <- demog_data_all_woe[ , !(names(demog_data_all_woe) %in% colums_to_be_dropped)]

str(demog_data_all_woe)

#********************************************************************#
#********************************************************************#
#********************************************************************#


#Credit bureau data


View(credit_data_matched)
nrow(credit_data_matched) #69554 rows
ncol(credit_data_matched) #19 columns
head(credit_data_matched)
tail(credit_data_matched)
str(credit_data_matched)
summary(credit_data_matched)

# Checking missing values
sum(is.na(credit_data_matched)) # 1558 blanks

#Check duplicate rows

length(unique(tolower(credit_data_matched$Application.ID))) # 71292, 3 duplicates as total # of rows are 71295 

dup_cred <-  duplicated(credit_data_matched$Application.ID) 
sum(dup_cred)


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
  App_ID = NewID,
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

colnames(credit_data_matched)[colSums(is.na(credit_data_matched)) > 0]

data.frame(colSums(is.na(credit_data_matched)))
colSums.is.na.credit_data_matched..
#App_ID                                              0
#No_of_90dpd_L6M                                     0
#No_of_60dpd_L6M                                     0
#No_of_30dpd_L6M                                     0
#No_of_90dpd_L12M                                    0
#No_of_60dpd_L12M                                    0
#No_of_30dpd_L12M                                    0
#Avg_CC_Util_L12M                                    0
#No_of_trades_opnd_L6M                               1015
#No_of_trades_opnd_L12M                              0
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
quantile(credit_data_matched$Avg_CC_Util_L12M,seq(0,1,0.001)) #not sure if we need to cap this variable


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
quantile(credit_data_matched$Outstanding_Bal,seq(0,1,0.001)) #income is 4.5 for first 5%, hence taking benchmark

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

#binning based on woe and IV for columns with NA values
str(credit_data_matched)
binning <- woe.binning(credit_data_matched, "Performance_Tag", c("No_of_trades_opnd_L6M", "Presence_of_opn_HL", "Outstanding_Bal",
                                                                 "Avg_CC_Util_L12M"))

binning[1,]
#No_of_trades_opnd_L6M

#               woe cutpoints.final cutpoints.final[-1] iv.total.final    1     0   col.perc.a   col.perc.b      iv.bins
#(-Inf,1] -54.21047            -Inf                   1      0.1685564  803 31292 2.731473e-01 0.4697101903 1.065577e-01
#(1, Inf]  31.53795               1                 Inf      0.1685564 2137 35321 7.267527e-01 0.5301748323 6.199663e-02
#Missing  -13.98651             Inf             Missing      0.1685564    0     1 9.997001e-05 0.0001149774 2.099005e-06


#iv.total.final 
#0.1685564

binning[2,]
#Presence_of_opn_HL

#                woe cutpoints.final cutpoints.final[-1] iv.total.final    1     0  col.perc.a  col.perc.b      iv.bins
#(-Inf,0]   7.283494            -Inf                   0     0.01722315 2326 49000 0.791156463 0.735581109 0.0040478276
#(0, Inf] -23.402025               0                 Inf     0.01722315  606 17351 0.206122449 0.260470772 0.0127186081
#Missing  -37.220731             Inf             Missing     0.01722315    8   263 0.002721088 0.003948119 0.0004567097


#iv.total.final 
#0.01722315 

binning[3,]
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
credit_data_matched_all_woe<- woe.binning.deploy(credit_data_matched, binning, add.woe.or.dum.var='woe')

#********************************************************************#

#removing variables repalced by woe from the data frame
str(credit_data_matched_all_woe)

colums_to_be_dropped <- c("No_of_trades_opnd_L6M", "Presence_of_opn_HL", "Outstanding_Bal","Avg_CC_Util_L12M", "Avg_CC_Util_L12M.binned",
                          "No_of_trades_opnd_L6M.binned", "Presence_of_opn_HL.binned", "Outstanding_Bal.binned")

credit_data_matched_all_woe <- credit_data_matched_all_woe[ , !(names(credit_data_matched_all_woe) %in% colums_to_be_dropped)]


sum(is.na(credit_data_matched_all_woe$Avg_CC_Util_L12M))

str(credit_data_matched_all_woe)
str(demog_data_all_woe)
#********************************************************************#
#********************************************************************#
#********************************************************************#

#Merging credit beureu data and demographic data based on application id

#renaming NewID column in demographic data to App_ID

colnames(demog_data_all_woe)[which(names(demog_data_all_woe) == "NewID")] <- "App_ID"

merged_dataframe_all_woe <- merge(x=demog_data_all_woe,y=credit_data_matched_all_woe,by="App_ID")

#Generic stats
str(merged_dataframe_all_woe)
nrow(merged_dataframe_all_woe) #69554

#checking if both the performance tag values are same
identical(merged_dataframe_all_woe$Performance.Tag, merged_dataframe_all_woe$Performance_Tag)

#removing a performance tag variable as its redundant
merged_dataframe_all_woe <- merged_dataframe_all_woe[ , !(names(merged_dataframe_all_woe) %in% c("Performance.Tag"))]


#Data set with all woes ready
View(merged_dataframe_all_woe)


#********************************************************************#
#***** Training a logistic model on the data ************************#
#********************************************************************#

# removing App_ID as it is irrelevant to the Model

ready_data_set <- merged_dataframe_all_woe[ , !(names(merged_dataframe_all_woe) %in% c("App_ID"))]

#splitting data into train and test
set.seed(1)

split_indices <- sample.split(ready_data_set$Performance_Tag, SplitRatio = 0.70)

train <- ready_data_set[split_indices, ]

test <- ready_data_set[!split_indices, ]

nrow(train)/nrow(ready_data_set)

nrow(test)/nrow(ready_data_set)


#********************************************************************#
#Training a Logistic model
#********************************************************************#

logistic_1 <- glm(Performance_Tag ~ ., family = "binomial", data = train)

summary(logistic_1)


#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

 logistic_2 <- stepAIC(logistic_1, direction = "both")

summary(logistic_2)
# stepAIC has removed some variables and only the following ones remain

logistic_3 <- glm(formula = Performance_Tag ~ Income + woe.Profession.binned + No_of_60dpd_L6M + 
                    No_of_30dpd_L6M + No_of_90dpd_L12M + Avg_CC_Util_L12M + No_of_PL_trades_opnd_L12M + 
                    No_of_Inq_ex_HLAL_L6M + No_of_Inq_ex_HLAL_L12M + Total_No_of_Trades + 
                    woe.No_of_trades_opnd_L6M.binned + woe.Presence_of_opn_HL.binned, 
                  family = "binomial", data = train)

vif(logistic_3)
summary(logistic_3)

#-----------------------------------------------------------
#removing No_of_60dpd_L6M

logistic_4 <- glm(formula = Performance_Tag ~ Income + woe.Profession.binned + 
                    No_of_30dpd_L6M + No_of_90dpd_L12M + Avg_CC_Util_L12M + No_of_PL_trades_opnd_L12M + 
                    No_of_Inq_ex_HLAL_L6M + No_of_Inq_ex_HLAL_L12M + Total_No_of_Trades + 
                    woe.No_of_trades_opnd_L6M.binned + woe.Presence_of_opn_HL.binned, 
                  family = "binomial", data = train)

vif(logistic_4)
summary(logistic_4)


#-----------------------------------------------------------
#removing No_of_Inq_ex_HLAL_L6M

logistic_5 <- glm(formula = Performance_Tag ~ Income + woe.Profession.binned + 
                    No_of_30dpd_L6M + No_of_90dpd_L12M + Avg_CC_Util_L12M + No_of_PL_trades_opnd_L12M + 
                    No_of_Inq_ex_HLAL_L12M + Total_No_of_Trades + 
                    woe.No_of_trades_opnd_L6M.binned + woe.Presence_of_opn_HL.binned, 
                  family = "binomial", data = train)

vif(logistic_5)
summary(logistic_5)


#-----------------------------------------------------------
#removing Income

logistic_6 <- glm(formula = Performance_Tag ~ woe.Profession.binned + 
                    No_of_30dpd_L6M + No_of_90dpd_L12M + Avg_CC_Util_L12M + No_of_PL_trades_opnd_L12M + 
                    No_of_Inq_ex_HLAL_L12M + Total_No_of_Trades + 
                    woe.No_of_trades_opnd_L6M.binned + woe.Presence_of_opn_HL.binned, 
                  family = "binomial", data = train)

vif(logistic_6)
summary(logistic_6)

#-----------------------------------------------------------
#removing woe.Presence_of_opn_HL.binned

logistic_7 <- glm(formula = Performance_Tag ~ woe.Profession.binned + 
                    No_of_30dpd_L6M + No_of_90dpd_L12M + Avg_CC_Util_L12M + No_of_PL_trades_opnd_L12M + 
                    No_of_Inq_ex_HLAL_L12M + Total_No_of_Trades + 
                    woe.No_of_trades_opnd_L6M.binned , 
                  family = "binomial", data = train)

vif(logistic_7)
summary(logistic_7)

#-----------------------------------------------------------
#removing Total_No_of_Trades

logistic_8 <- glm(formula = Performance_Tag ~ woe.Profession.binned + 
                    No_of_30dpd_L6M + No_of_90dpd_L12M + Avg_CC_Util_L12M + No_of_PL_trades_opnd_L12M + 
                    No_of_Inq_ex_HLAL_L12M + 
                    woe.No_of_trades_opnd_L6M.binned , 
                  family = "binomial", data = train)

vif(logistic_8)
summary(logistic_8)

#-----------------------------------------------------------
#removing No_of_90dpd_L12M
logistic_9 <- glm(formula = Performance_Tag ~ woe.Profession.binned + 
                    No_of_30dpd_L6M + Avg_CC_Util_L12M + No_of_PL_trades_opnd_L12M + 
                    No_of_Inq_ex_HLAL_L12M + 
                    woe.No_of_trades_opnd_L6M.binned , 
                  family = "binomial", data = train)

vif(logistic_9)
summary(logistic_9)


#-----------------------------------------------------------
#removing No_of_PL_trades_opnd_L12M

logistic_10 <- glm(formula = Performance_Tag ~ woe.Profession.binned + 
                    No_of_30dpd_L6M + Avg_CC_Util_L12M + 
                    No_of_Inq_ex_HLAL_L12M + 
                    woe.No_of_trades_opnd_L6M.binned , 
                  family = "binomial", data = train)

vif(logistic_10)
summary(logistic_10)
#-----------------------------------------------------------

predictions_logit <- predict(logistic_10, newdata = test)
summary(predictions_logit)


predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

unique(predicted_response)

test$Performance_Tag <- factor(ifelse(test$Performance_Tag == 1, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, as.factor(test$Performance_Tag), positive = "no")

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

