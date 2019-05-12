#********************************************************************#
#********************************************************************#
### Load credit bureau and demographic dataset  
credit_data <- read.csv(file.choose())
demog_data <- read.csv(file.choose())


#*******************************************************************************#
# Data Understanding and Data Prepration - Begins
#*******************************************************************************#

View(credit_data)
nrow(credit_data) #71295 rows
ncol(credit_data) #19 columns
head(credit_data)
tail(credit_data)
str(credit_data)
summary(credit_data)

# Checking missing values
sum(is.na(credit_data)) # 3028 blanks

#Check duplicate rows

length(unique(tolower(credit_data$Application.ID))) # 71292, 3 duplicates as total # of rows are 71295 

dup_cred <-  duplicated(credit_data$Application.ID) 
sum(dup_cred)


#Check application id for the duplicate records
dup_cred_id <- credit_data[duplicated(credit_data$Application.ID),]$Application.ID

#View duplicate application ids
dup_cred_id

#Remove duplicates
credit_data <- credit_data[!credit_data$Application.ID %in% dup_cred_id,]

dim(credit_data)

dim(credit_data)
#[1] 71289    19


#rename long names with short name for ease of coding
#********************************************************************#
#********************************************************************#

#get column names - credit 
colnames(credit_data)

credit_data = credit_data %>% rename(
  App_ID = Application.ID,
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

colnames(credit_data)

#********************************************************************#
#********************************************************************#                                                
table(credit_data$Performance_Tag)
#0     1 
#66917  2947 

summary(credit_data) # many na values are there need to check varable wise

#********************************************************************#
#********************************************************************#
#Storing the Details of data having performance tag as NA

#Check NA for perf variable
perf_na=sum(is.na(credit_data$Performance_Tag)) 
perf_na #1403- most likely to be rejected accounts as mentioned in the problem statement

#Get all the records with NA values for perf variable
Chk_NA <- credit_data[which(is.na(credit_data$Performance_Tag)),]
nrow(Chk_NA) 

#Rmove NA values, its around 2% of the total base
rows_NA <- apply(credit_data,1, function(x){any(is.na(x))})
credit_data <- credit_data[!rows_NA,]

#check data after removing NA
sum(is.na(credit_data$Performance_Tag)) # 0 na values

#need to check other variable to find NA
sum(is.na(credit_data)) # 0 na values

#********************************************************************#
#********************************************************************#

data.frame(colSums(is.na(credit_data)))
colSums.is.na.credit_data..
#App_ID                                              0
#No_of_90dpd_L6M                                     0
#No_of_60dpd_L6M                                     0
#No_of_30dpd_L6M                                     0
#No_of_90dpd_L12M                                    0
#No_of_60dpd_L12M                                    0
#No_of_30dpd_L12M                                    0
#Avg_CC_Util_L12M                                    0
#No_of_trades_opnd_L6M                               0
#No_of_trades_opnd_L12M                              0
#No_of_PL_trades_opnd_L6M                            0
#No_of_PL_trades_opnd_L12M                           0
#No_of_Inq_ex_HLAL_L6M                               0
#No_of_Inq_ex_HLAL_L12M                              0
#Presence_of_opn_HL                                  0
#Outstanding_Bal                                     0
#Total_No_of_Trades                                  0
#Presence_of_open_AL                                 0
#Performance_Tag                                     0

###################################################
# Variable wise analysis for all char variable- Univariate Analysis
##################################################

#********************************************************************#

#No_of_90dpd_L6M 

table(credit_data$No_of_90dpd_L6M) 
#0     1     2     3 
#53667 13201  1766   207 

summary(credit_data$No_of_90dpd_L6M) 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.2521  0.0000  3.0000

histogram(credit_data$No_of_90dpd_L6M) 
# Most people have no such overdues 

#********************************************************************#

table(credit_data$No_of_60dpd_L6M) 
#0     1     2     3     4     5 
#50879 11123  4903  1461   405    70 

#No_of_60dpd_L6M 
summary(credit_data$No_of_60dpd_L6M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.3963  1.0000  5.0000 

histogram(credit_data$No_of_60dpd_L6M) 
# Most people have no such overdues  
#********************************************************************#

#No_of_30dpd_L6M 
table(credit_data$No_of_30dpd_L6M)  
#0     1     2     3     4     5     6     7 
#49107  9498  5888  2822  1036   380    95    15

summary(credit_data$No_of_30dpd_L6M)   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.5296  1.0000  7.0000

histogram(credit_data$No_of_30dpd_L6M) 

#********************************************************************#

#No_of_90dpd_L12M 
table(credit_data$No_of_90dpd_L12M)  
#0     1     2     3     4     5 
#49501 11653  6149  1236   267    35 

summary(credit_data$No_of_90dpd_L12M) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.4198  1.0000  5.0000 

histogram(credit_data$No_of_90dpd_L12M) 
#********************************************************************#

#No_of_60dpd_L12M 
table(credit_data$No_of_60dpd_L12M)  
#0     1     2     3     4     5     6     7 
#44963 12727  6408  3192  1041   393   110     7 

summary(credit_data$No_of_60dpd_L12M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.6095  1.0000  7.0000 

histogram(credit_data$No_of_60dpd_L12M) 
#********************************************************************#

#No_of_30dpd_L12M 
table(credit_data$No_of_30dpd_L12M)  
#0     1     2     3     4     5     6     7     8     9 
#43954 11386  6110  4128  1918   846   369   106    23     1 

summary(credit_data$No_of_30dpd_L12M)   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.7414  1.0000  9.0000 

histogram(credit_data$No_of_30dpd_L12M) 
boxplot(credit_data$No_of_30dpd_L12M,ylab = "No_of_30dpd_L12M",col = "blue")
#********************************************************************#

#No_of_trades_opnd_L6M 

summary(credit_data$No_of_trades_opnd_L6M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   2.000   2.318   3.000  12.000 

histogram(credit_data$No_of_trades_opnd_L6M) 
boxplot(credit_data$No_of_trades_opnd_L6M,ylab = "No_of_trades_opnd_L6M",col = "blue")
# most users have 0-4 trades opened in last 6 mon.
# Outlier do exist.

#********************************************************************#

#No_of_trades_opnd_L12M 
summary(credit_data$No_of_trades_opnd_L12M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   2.000   5.000   5.867   9.000  28.000 

histogram(credit_data$No_of_trades_opnd_L12M) 
boxplot(credit_data$No_of_trades_opnd_L12M,ylab = "No_of_trades_opnd_L12M",col = "blue")
# most users have 0-10 trades opened in last 12 mon.
# Outlier do exist.

#********************************************************************#

#No_of_PL_trades_opnd_L6M  
summary(credit_data$No_of_PL_trades_opnd_L6M )   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   1.000   1.206   2.000   6.000 


histogram(credit_data$No_of_PL_trades_opnd_L6M) 
boxplot(credit_data$No_of_PL_trades_opnd_L6M,ylab = "No_of_PL_trades_opnd_L6M",col = "blue")
# most users have 0-3 PL opened in last 6 mon.
# Very few Outlier are there.


#********************************************************************#

#No_of_PL_trades_opnd_L12M 
table(credit_data$No_of_PL_trades_opnd_L12M)  
#0     1     2     3     4     5     6     7     8     9    10    11    12 
#24834  6639  6826  8126  7897  6179  4015  2221  1172   601   255    66    10 

summary(credit_data$No_of_PL_trades_opnd_L12M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   2.000   2.396   4.000  12.000 

histogram(credit_data$No_of_PL_trades_opnd_L12M) 
boxplot(credit_data$No_of_PL_trades_opnd_L12M,ylab = "No_of_PL_trades_opnd_L12M",col = "blue")
# most users have 0-6 trades opened in last 12 mon.
# Outlier might be there.

#********************************************************************#

#No_of_Inq_ex_HLAL_L6M  
table(credit_data$No_of_Inq_ex_HLAL_L6M )  
#0     1     2     3     4     5     6     7     8     9    10 
#24117 13142 12805  7247  4244  3019  1750  1149   835   425   108


summary(credit_data$No_of_Inq_ex_HLAL_L6M ) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   1.000   1.782   3.000  10.000 

histogram(credit_data$No_of_Inq_ex_HLAL_L6M) 
boxplot(credit_data$No_of_Inq_ex_HLAL_L6M,ylab = "No_of_Inq_ex_HLAL_L6M",col = "blue")
#********************************************************************#

#Presence_of_opn_HL  
table(credit_data$Presence_of_opn_HL )  
#0     1 
#50778 18063 

histogram(credit_data$Presence_of_opn_HL) 
#********************************************************************#

#Presence_of_open_AL  
table(credit_data$Presence_of_open_AL ) 
#0     1 
#62912  5929 

histogram(credit_data$Presence_of_open_AL) 

#********************************************************************#

#Presence_of_open_AL  
table(credit_data$Presence_of_open_AL )  
#0     1 
#62912  5929 
histogram(credit_data$Presence_of_open_AL) 

#********************************************************************#

table(credit_data$Performance_Tag) 
#Plot shows age 0 amd negative 
histogram(credit_data$Performance_Tag) 

#********************************************************************#

#No_of_Inq_ex_HLAL_L12M 
summary(credit_data$No_of_Inq_ex_HLAL_L12M)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   3.000   3.569   5.000  20.000  

histogram(credit_data$No_of_Inq_ex_HLAL_L12M) 
boxplot(credit_data$No_of_PL_trades_opnd_L12M,ylab = "No_of_Inq_ex_HLAL_L12M",col = "blue")

#checks quantiles
quantile(credit_data$No_of_Inq_ex_HLAL_L12M,seq(0,1,0.001))# not sure if we need to cap this variable

#********************************************************************#
#Avg_CC_Util_L12M   
summary(credit_data$Avg_CC_Util_L12M  )   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    8.00   15.00   29.26   45.00  113.00 

histogram(credit_data$Avg_CC_Util_L12M) 
boxplot(credit_data$Avg_CC_Util_L12M,ylab = "Avg_CC_Util_L12M",col = "blue")
## most users are utilizing only upto 20% of card upper limit, 
## population size with proper 25 to 60 % card utilization is similar
## Left skewed ..outliers do exist.

#checks quantiles
quantile(credit_data$Avg_CC_Util_L12M,seq(0,1,0.001)) #not sure if we need to cap this variable


#********************************************************************#
#Total_No_of_Trades   
summary(credit_data$Total_No_of_Trades)  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   3.000   6.000   8.293  10.000  44.000 

histogram(credit_data$Total_No_of_Trades) 
boxplot(credit_data$Total_No_of_Trades,ylab = "Total_No_of_Trades",col = "blue")

# most users have 0-10 trades in total
# Outlier are there.


#checks quantiles
quantile(credit_data$Total_No_of_Trades,seq(0,1,0.001)) 

#********************************************************************#

#Outstanding_Bal 
summary(credit_data$Outstanding_Bal)   
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0  213488  777938 1266382 2930252 5218801 

#checks quantiles
quantile(credit_data$Outstanding_Bal,seq(0,1,0.001)) #income is 4.5 for first 5%, hence taking benchmark

boxplot(credit_data$Outstanding_Bal,ylab = "Outstanding_Bal",col = "blue")
# 0-200000 range higher no of users
# 300k upwards lower no of users


#********************************************************************#

#Ploting the histogram of No_of_90dpd_L6M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_90dpd_L6M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins=20)+
  labs(title="Distribution of No_of_90dpd_L6M vs Performance_Tag",x="90dpd_L6M",y="Count",fill="target")  


#Ploting the histogram of No_of_60dpd_L6M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_60dpd_L6M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_60dpd_L6M vs Performance_Tag",x="60dpd_L6M",y="Count",fill="target") 


#No_of_30dpd_L6M
#Ploting the histogram of No_of_30dpd_L6M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_30dpd_L6M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_30dpd_L6M vs Performance_Tag",x="30dpd_L6M",y="Count",fill="target") 

#customer moving from 60 to 90
#Ploting the histogram of No_of_60dpd_L6M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_90dpd_L6M,fill=factor(credit_data$No_of_60dpd_L6M)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_90dpd_L6M vs No_of_60dpd_L6M",x="90dpd_L6M",y="Count",fill="No_of_60dpd_L6M")  

#********************************************************************#
#********************************************************************#

#Ploting the histogram of No_of_90dpd_L12M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_90dpd_L12M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_90dpd_L12M vs Performance_Tag",x="90dpd_L12M",y="Count",fill="target")  


#Ploting the histogram of No_of_60dpd_L12M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_60dpd_L12M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_60dpd_L12M vs Performance_Tag",x="60dpd_L12M",y="Count",fill="target")  


#No_of_30dpd_L6M
#Ploting the histogram of No_of_30dpd_L6M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_30dpd_L12M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_30dpd_L12M vs Performance_Tag",x="30dpd_L12M",y="Count",fill="target") 

#********************************************************************#

#Ploting the histogram of No_of_90dpd_L12M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_90dpd_L12M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_90dpd_L12M vs Performance_Tag",x="90dpd_L12M",y="Count",fill="target")  


#Ploting the histogram of No_of_60dpd_L12M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_60dpd_L12M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_60dpd_L12M vs Performance_Tag",x="60dpd_L12M",y="Count",fill="target")  


#No_of_30dpd_L6M
#Ploting the histogram of No_of_30dpd_L6M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_30dpd_L12M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_30dpd_L12M vs Performance_Tag",x="30dpd_L12M",y="Count",fill="target") 

#********************************************************************#

#Ploting the histogram of No_of_trades_opnd_L6M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_trades_opnd_L6M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_trades_opnd_L6M vs Performance_Tag",x="No_of_trades_opnd_L6M",y="Count",fill="target")  


#Ploting the histogram of No_of_trades_opnd_L12M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_trades_opnd_L12M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_trades_opnd_L12M vs Performance_Tag",x="trades_opnd_L12M",y="Count",fill="target")  


#No_of_trades_opnd_L6M vs No_of_trades_opnd_L12M
ggplot(data=credit_data,aes(x=No_of_trades_opnd_L12M,fill=factor(credit_data$No_of_trades_opnd_L6M)))+geom_histogram(bins = 10)+
  labs(title="Distribution of No_of_trades_opnd_L12M vs No_of_trades_opnd_L6M",x="30dpd_L12M",y="Count",fill="target") 

#********************************************************************#

#Ploting the histogram of No_of_PL_trades_opnd_L6M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_PL_trades_opnd_L6M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_PL_trades_opnd_L6M vs Performance_Tag",x="No_of_PL_trades_opnd_L6M",y="Count",fill="target")  


#Ploting the histogram of No_of_trades_opnd_L12M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_PL_trades_opnd_L12M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 20)+
  labs(title="Distribution of No_of_PL_trades_opnd_L12M vs Performance_Tag",x="No_of_PL_trades_opnd_L12M",y="Count",fill="target")  


#********************************************************************#

#Ploting the histogram of No_of_Inq_ex_HLAL_L6M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_Inq_ex_HLAL_L6M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 25)+
  labs(title="Distribution of No_of_Inq_ex_HLAL_L6M vs Performance_Tag",x="No_of_Inq_ex_HLAL_L6M",y="Count",fill="target")  


#Ploting the histogram of No_of_Inq_ex_HLAL_L12M vs Performance_Tag
ggplot(data=credit_data,aes(x=No_of_Inq_ex_HLAL_L12M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of No_of_Inq_ex_HLAL_L12M vs Performance_Tag",x="No_of_Inq_ex_HLAL_L12M",y="Count",fill="target")  


#********************************************************************#

#Ploting the histogram of Presence_of_opn_HL vs Performance_Tag
ggplot(data=credit_data,aes(x=Presence_of_opn_HL,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of Presence_of_opn_HL vs Performance_Tag",x="Presence_of_opn_HL",y="Count",fill="target")  

#Ploting the histogram of Presence_of_open_AL vs Performance_Tag
ggplot(data=credit_data,aes(x=Presence_of_open_AL,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of Presence_of_open_AL vs Performance_Tag",x="Presence_of_open_AL",y="Count",fill="target")  


#Ploting the histogram of Total_No_of_Trades vs Performance_Tag
ggplot(data=credit_data,aes(x=Total_No_of_Trades,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of Total_No_of_Trades vs Performance_Tag",x="Total_No_of_Trades",y="Count",fill="target")  

#********************************************************************#

#Ploting the histogram of Presence_of_opn_HL vs Performance_Tag
ggplot(data=credit_data,aes(x=Presence_of_opn_HL,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of Outstanding_Bal vs Performance_Tag",x="Outstanding_Bal",y="Count",fill="target")  

#Ploting the histogram of Presence_of_open_AL vs Performance_Tag
ggplot(data=credit_data,aes(x=Avg_CC_Util_L12M,fill=factor(credit_data$Performance_Tag)))+geom_histogram(bins = 30)+
  labs(title="Distribution of Avg_CC_Util_L12M vs Performance_Tag",x="Avg_CC_Util_L12M",y="Count",fill="target")  


#********************************************************************#
# Check correlation

nums_var <- credit_data[,-19] #remove perfomance tag
nums_var <- nums_var[,-1] #remove appl id 

corrs = round(cor(nums_var, use = "pairwise.complete.obs"), 2)
View(corrs) #not sure how to use this information in the model

#********************************************************************#
print(corrs)

#********************************************************************#

#Calculating WOE values for evary variable
library(Information) # to calculate woe

IV_allvars <- create_infotables(data=credit_data[,-1],y="Performance_Tag",parallel = F)

IV_allvars$Summary
#********************************************************************#  
