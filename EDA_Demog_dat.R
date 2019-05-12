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

# Binning the age variable and store it into "binning.age".

demog_data$binning.age <- as.factor(cut(demog_data$Age, breaks = c(18, 25, 35, 45, 55, 65)))

# Change the response value to numeric

demog_data$Performance.Tag <- as.numeric(demog_data$Performance.Tag)

# Check the numeric value of Performance.Tag in each bucket

agg_age <- merge(aggregate(Performance.Tag ~ binning.age, demog_data, mean),aggregate(Performance.Tag~binning.age, demog_data, sum),by = "binning.age") 

# Adding No.of_IDs
count <- data.frame(table(demog_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age_range", "default_rate", "count_defaults","No.of_IDs")

# Round Off the values

agg_age$default_rate <- format(round(agg_age$default_rate, 4))

agg_age
#   age_range default_rate count_defaults No.of_IDs
# 1   (18,25]       0.0246              8       325
# 2   (25,35]       0.0430            533     12385
# 3   (35,45]       0.0433           1005     23207
# 4   (45,55]       0.0399            900     22552
# 5   (55,65]       0.0447            494     11062

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

# Binning the Income variable and store it into "binning.income".

demog_data$binning.income <- as.factor(cut(demog_data$Income, breaks = c(0, 10, 20, 30, 40, 50, 60)))

# Check the numeric value of Performance.Tag in each bucket

agg_income <- merge(aggregate(Performance.Tag ~ binning.income, demog_data, mean),aggregate(Performance.Tag~binning.income, demog_data, sum),by = "binning.income")

# Adding No.of_IDs
count <- data.frame(table(demog_data$binning.income))
count <- count[,-1]
agg_income <- cbind(agg_income,count)

# changing column name of each variables in agg_income dataframe

colnames(agg_income) <- c("income_range", "default_rate", "count_defaults","No.of_IDs")

# Round Off the values

agg_income$default_rate <- format(round(agg_income$default_rate, 4))

agg_income

#   income_range default_rate count_defaults No.of_IDs
# 1       (0,10]       0.0558            709     12698
# 2      (10,20]       0.0453            603     13303
# 3      (20,30]       0.0443            600     13546
# 4      (30,40]       0.0355            483     13592
# 5      (40,50]       0.0355            384     10815
# 6      (50,60]       0.0288            161      5600
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

#Question should we treat outliers here?

# Binning the No of months in current residence variable and store it into "bin.res_tenor".

demog_data$bin.res_tenor <- as.factor(cut(demog_data$No.of.months.in.current.residence, breaks = c(0, 8, 24, 40, 56, 72, 88, 104, 130)))

# Check the numeric value of Performance.Tag in each bucket

agg_res_tenor <- merge(aggregate(Performance.Tag ~ bin.res_tenor, demog_data, mean),aggregate(Performance.Tag~bin.res_tenor, demog_data, sum),by = "bin.res_tenor")

# Adding No.of_IDs
count <- data.frame(table(demog_data$bin.res_tenor))
count <- count[,-1]
agg_res_tenor <- cbind(agg_res_tenor,count)

# changing column name of each variables in agg_res_tenor dataframe

colnames(agg_res_tenor) <- c("res_tenor_range", "default_rate", "count_defaults","No.of_IDs")

# Round Off the values

agg_res_tenor$default_rate <- format(round(agg_res_tenor$default_rate, 4))

agg_res_tenor
#   res_tenor_range default_rate count_defaults No.of_IDs
# 1           (0,8]       0.0320           1096     34263
# 2       (104,130]       0.0402            203      5904
# 3         (24,40]       0.0607            340      5600
# 4         (40,56]       0.0548            286      5216
# 5         (56,72]       0.0442            207      4684
# 6         (72,88]       0.0497            219      4407
# 7          (8,24]       0.0681            402      4426
# 8        (88,104]       0.0423            187      5054

# Let's see the default rate of each res_tenor bucket in the plot

#Ploting the histogram of Res-tenor vs Performance_Tag
ggplot(agg_res_tenor, aes(res_tenor_range,default_rate ,label = No.of_IDs)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + labs(title="Default by Res_tenor bin",x="Res_tenor_range",y="Def_rate",fill="#ofApps")

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

#Question should we treat outliers here?

# Binning the No of months in current Co variable and store it into "bin.curCo_tenor".

demog_data$bin.curCo_tenor <- as.factor(cut(demog_data$No.of.months.in.current.company, breaks = c(0,10, 20, 30, 40, 50, 60, 150)))

# Check the numeric value of Performance.Tag in each bucket

agg_curCo_tenor <- merge(aggregate(Performance.Tag ~ bin.curCo_tenor, demog_data, mean),aggregate(Performance.Tag~bin.curCo_tenor, demog_data, sum),by = "bin.curCo_tenor")

# Adding No.of_IDs
count <- data.frame(table(demog_data$bin.curCo_tenor))
count <- count[,-1]
agg_curCo_tenor <- cbind(agg_curCo_tenor,count)

# changing column name of each variables in agg_curCo_tenor dataframe

colnames(agg_curCo_tenor) <- c("curCo_tenor_range", "default_rate", "count_defaults","No.of_IDs")

# Round Off the values

agg_curCo_tenor$default_rate <- format(round(agg_curCo_tenor$default_rate, 4))

agg_curCo_tenor

# curCo_tenor_range default_rate count_defaults No.of_IDs
# 1            (0,10]       0.0485            559     11535
# 2           (10,20]       0.0501            490      9785
# 3           (20,30]       0.0413            413     10009
# 4           (30,40]       0.0423            429     10149
# 5           (40,50]       0.0378            385     10196
# 6           (50,60]       0.0317            318     10030
# 7          (60,150]       0.0441            346      7850

# Let's see the default rate of each curCo_tenor bucket in the plot

#Ploting the histogram of Curco-tenor vs Performance_Tag
ggplot(agg_curCo_tenor, aes(curCo_tenor_range,default_rate ,label = No.of_IDs)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + labs(title="Default by curCo_tenor bin",x="curCo_tenor_range",y="Def_rate",fill="#ofApps")

#default rates are higher when curCo tenor is low 
#40-60m CurCo tenor is the sweet sppot for low default

#********************************************************************#


#Calculating WOE values for evary variable
library(Information) # to calculate woe

IV_allvars <- create_infotables(data=demog_data[,-1],y="Performance.Tag",parallel = F)

IV_allvars$Summary
#********************************************************************#  


nums_var <- demog_data[,-1] #remove Application id

cor(nums_var[,unlist(lapply(nums_var, is.numeric))])

