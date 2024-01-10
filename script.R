#get working directory
getwd()
#set wd to ensure working directory is in this file loc
setwd("C:/Users/Administrator.NDESKTOP/d206-churn.dictionary.files")
#create df using churn_raw_data.csv
df <- read.csv("churn_raw_data.csv")
#check data types using structure function
str(df)
#check null values
sum(is.na(df))
# check null cols
colSums(is.na(df))
#check null values by row
rowSums(is.na(df))
#check dimensions of data frame
dim(df)
df[,'item1']
is.na(df[,'Age'])
#install ggplot2 for data visualization
#install.packages("ggplot2")
library(ggplot2)
#install dplyr for data manipulation
#install.packages("dplyr")
library(dplyr)


#z score for X (row identifier) 
df$X_zScore <- scale(x=df$X)
#identify X outliers using zscore
X_outliers <- df[which(df$X_zScore < -3 | df$X_zScore > 3),]
#calculate quantity of outliers 
num_of_outliers <- nrow(X_outliers)
print(num_of_outliers) #output = 0
#sort data by X outliers
df_X_sort <- df[order(-df$X_zScore),]
#print head of df_X_sort
head(df_X_sort)
#print first and last 10 rows
df_X_sort[1:10,c(1,53)]
df_X_sort[9990:10000,c(1,53)]
#print first 10 rows, X and churn column
df_X_sort[1:10,c(1,22)]
#print first 10 rows,X, techie and churn column
df_X_sort[1:20,c(1,22,27)]
#check for null/missing zscores
sum(is.na(df$X_zScore))
#z score for CaseOrder 
df$CaseOrder_zScore <- scale(x=df$CaseOrder)
#identify CaseOrder outliers using zscore
CaseOrder_outliers <- df[which(df$CaseOrder_zScore < -3 | df$CaseOrder_zScore > 3),]
#calculate quantity of outliers 
num_of_outliers <- nrow(CaseOrder_outliers) #output = 0
print(num_of_outliers)
#sort data by CaseOrder outliers
df_CaseOrder_sort <- df[order(-df$CaseOrder_zScore),]
#print head of df_CaseOrder_sort
head(df_CaseOrder_sort)
#print first and last 10
df_CaseOrder_sort[1:10,c(1,54)]
df_CaseOrder_sort[9990:10000,c(1,54)]
#print first 10 rows, CaseOrder and churn column
df_CaseOrder_sort[1:10,c(2,22)]
#print first 10 rows,CaseOrder, techie and churn column
df_CaseOrder_sort[1:20,c(2,22,27)]
#check for null/missing zscores
sum(is.na(df$CaseOrder_zScore))
#z score for Zip 
df$Zip_zScore <- scale(x=df$Zip)
#identify Zip outliers using zscore
Zip_outliers <- df[which(df$Zip_zScore < -3 | df$Zip_zScore > 3),]
#calculate quantity of outliers 
num_of_outliers <- nrow(Zip_outliers) #output = 0
print(num_of_outliers)
#sort data by Zip outliers
df_Zip_sort <- df[order(-df$Zip_zScore),]
#print head of df_Zip_sort
head(df_Zip_sort)
#print first 10 rows
df_Zip_sort[1:76,c(1,55)]
df_Zip_sort[9990:10000,c(1,55)]
#print first 10 rows, Zip and churn column
df_Zip_sort[1:10,c(8,22)]
#print first 10 rows,Zip, techie and churn column
df_Zip_sort[1:20,c(8,22,27)]
#check for null/missing zscores
sum(is.na(df$Zip_zScore))
#z score for Lat 
df$Lat_zScore <- scale(x=df$Lat)
#identify Lat outliers using zscore
Lat_outliers <- df[which(df$Lat_zScore < -3 | df$Lat_zScore > 3),]
#calculate quantity of outliers 
num_of_outliers <- nrow(Lat_outliers) #output = 151
print(num_of_outliers)
#sort data by Lat outliers
df_Lat_sort <- df[order(-df$Lat_zScore),]
#print head of df_Lat_sort
head(df_Lat_sort)
#print first 76 rows and last 76 rows in col 56 for Lat_zScore
df_Lat_sort[1:76,c(1,56)]
df_Lat_sort[9926:10000,c(1,56)]
#print first 10 rows, Lat and churn column
df_Lat_sort[1:10,c(9,22)]
#print first 10 rows,Lat, techie and churn column
df_Lat_sort[1:20,c(9,22,27)]
#check for null/missing zscores
sum(is.na(df$Lat_zScore))
#z score for Lng 
df$Lng_zScore <- scale(x=df$Lng)
#identify Lng outliers using zscore
Lng_outliers <- df[which(df$Lng_zScore < -3 | df$Lng_zScore > 3),]
#calculate quantity of outliers 
num_of_outliers <- nrow(Lng_outliers) #output = 102
print(num_of_outliers)
#sort data by Lng outliers
df_Lng_sort <- df[order(-df$Lng_zScore),]
#print head of df_Lng_sort
head(df_Lng_sort)
#print first and last 102 rows in col 57 for Lng_zScore
df_Lng_sort[9899:10000,c(1,57)]
#print first 10 rows, Lng and churn column
df_Lng_sort[1:10,c(10,22)]
#print first 10 rows,Lng, techie and churn column
df_Lng_sort[1:20,c(10,22,27)]
#check for null/missing zscores
sum(is.na(df$Lng_zScore))
#z score for Population 
df$Population_zScore <- scale(x=df$Population)
#identify Population outliers using zscore
Population_outliers <- df[which(df$Population_zScore < -3 | df$Population_zScore > 3),]
#calculate quantity of outliers 
num_of_outliers <- nrow(Population_outliers) #output = 219
print(num_of_outliers)
#sort data by Population outliers
df_Population_sort <- df[order(-df$Population_zScore),]
#print head of df_Population_sort
head(df_Population_sort)
#print first 219 rows of population zscore col
df_Population_sort[1:219,c(1,58)]
#print first 10 rows, Population and churn column
df_Population_sort[1:10,c(11,22)]
#print first 10 rows,Population, techie and churn column
df_Population_sort[1:20,c(11,22,27)]
#check for null/missing zscores
sum(is.na(df$Population_zScore))
#z score for Children 
df$Children_zScore <- scale(x=df$Children)
#identify outliers using zscore
Children_outliers <- df[which(df$Children_zScore < -3 | df$Children_zScore > 3),]
#calculate quantity of outliers 
num_of_outliers <- nrow(Children_outliers) #output = 302
print(num_of_outliers)
#sort data by Children outliers
df_Children_sort <- df[order(-df$Children_zScore),]
#print head of df_Children_sort
head(df_Children_sort)
#print first 10 rows 302 col 59
df_Children_sort[1:302,c(1,59)]
#print first 10 rows, Children and churn column
df_Children_sort[1:10,c(15,22)]
#print first 10 rows,Children, techie and churn column
df_Children_sort[1:20,c(15,22,27)]
#check for null/missing zscores
sum(is.na(df$Children_zScore))
#z score for age 
df$Age_zScore <- scale(x=df$Age)
#identify outliers using zscore
age_outliers <- df[which(df$Age_zScore < -3 | df$Age_zScore > 3),]
#calculate quantity of outliers 
num_of_outliers <- nrow(age_outliers) #output = 0
print(num_of_outliers)
#sort data by age outliers
df_age_sort <- df[order(-df$Age_zScore),]
#print head of df_age_sort
head(df_age_sort)
#print first 10 rows
df_age_sort[1:10,c(1,60)]
#print first 10 rows, age and churn column
df_age_sort[1:10,c(16,22)]
#print first 10 rows,age, techie and churn column
df_age_sort[1:20,c(16,22,27)]
#check for null/missing zscores
sum(is.na(df$Age_zScore)) #check this
#z score for Income 
df$Income_zScore <- scale(x=df$Income)
#identify Income outliers using zscore
Income_outliers <- df[which(df$Income_zScore < -3 | df$Income_zScore > 3),]
#calculate quantity of outliers 
num_of_outliers <- nrow(Income_outliers) #output = reality 180
print(num_of_outliers)
#sort data by age outliers
df_Income_sort <- df[order(-df$Income_zScore),]
#print head of df_Income_sort
head(df_Income_sort)
#print first 110 rows and last 70
df_Income_sort[1:180,c(1,61)]
# df_Income_sort[9705:10000,61]
#print first 10 rows, Income and churn column
df_Income_sort[1:10,c(19,22)]
#print first 10 rows,Income, techie and churn column
df_Income_sort[1:20,c(19,22,27)]
#check for null/missing zscores
sum(is.na(df$Income_zScore))
#z score for Outage_sec_perweek 
df$Outage_sec_perweek_zScore <- scale(x=df$Outage_sec_perweek)
#identify outliers using zscore
Outage_sec_perweek_outliers <- df[which(df$Outage_sec_perweek_zScore < -3 | df$Outage_sec_perweek_zScore > 3),]
#calculate quantity of outliers 
num_of_outliers <- nrow(Outage_sec_perweek_outliers) #output = 491
print(num_of_outliers)
#sort data by Outage_sec_perweek outliers
df_Outage_sec_perweek_sort <- df[order(-df$Outage_sec_perweek_zScore),]
#print head of df_Outage_sec_perweek_sort
head(df_Outage_sec_perweek_sort)
#print first 491 rows
df_Outage_sec_perweek_sort[1:491,c(1,62)]
#print first 10 rows, Outage_sec_perweek and churn column
df_Outage_sec_perweek_sort[1:10,c(23,22)]
#print first 10 rows,Outage_sec_perweek, techie and churn column
df_Outage_sec_perweek_sort[1:20,c(23,22,27)]
#check for null/missing zscores
sum(is.na(df$Outage_sec_perweek_zScore))
#z score for Email 
df$Email_zScore <- scale(df$Email)
#identify Email outliers using zscore
Email_outliers <- df[which(df$Email_zScore < -3 | df$Email_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(Email_outliers) #output = 12
print(num_of_outliers)
#sort data by Email outliers
df_Email_sort <- df[order(-df$Email_zScore),]
#print head of df_Email_sort
head(df_Email_sort)
#print 12 rows with outliers
df_Email_sort[1:3,c(1,63)]
df_Email_sort[9992:10000,63]
#print first 10 rows, Email and churn column
df_Email_sort[1:10,c(24,22)]
#print first 10 rows,Email, techie and churn column
df_Email_sort[1:20,c(24,22,27)]
#check for null/missing zscores
sum(is.na(df$Email_zScore))
#z score for Contacts 
df$Contacts_zScore <- scale(x=df$Contacts)
#identify Contacts outliers using zscore
Contacts_outliers <- df[which(df$Contacts_zScore < -3 | df$Contacts_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(Contacts_outliers) #output = 165
print(num_of_outliers)
#sort data by Contacts outliers
df_Contacts_sort <- df[order(-df$Contacts_zScore),]
#print head of df_Contacts_sort
head(df_Contacts_sort)
#print first 165 rows
df_Contacts_sort[1:165,c(1,64)]
#print first 10 rows, Contacts and churn column
df_Contacts_sort[1:10,c(25,22)]
#print first 10 rows,Contacts, techie and churn column
df_Contacts_sort[1:20,c(25,22,27)]
#check for null/missing zscores
sum(is.na(df$Contacts_zScore))
#z score for Yearly_equip_failure 
df$Yearly_equip_failure_zScore <- scale(x=df$Yearly_equip_failure)
#identify Yearly_equip_failure outliers using zscore
Yearly_equip_failure_outliers <- df[which(df$Yearly_equip_failure_zScore < -3 | df$Yearly_equip_failure_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(Yearly_equip_failure_outliers) #output = 94
print(num_of_outliers)
#sort data by Yearly_equip_failure outliers
df_Yearly_equip_failure_sort <- df[order(-df$Yearly_equip_failure_zScore),]
#print head of df_Yearly_equip_failure_sort
head(df_Yearly_equip_failure_sort)
#print first 94 rows
df_Yearly_equip_failure_sort[1:94,c(1,65)]
#print first 10 rows, Yearly_equip_failure and churn column
df_Yearly_equip_failure_sort[1:10,c(26,22)]
#print first 10 rows,Yearly_equip_failure, techie and churn column
df_Yearly_equip_failure_sort[1:20,c(26,22,27)]
#check for null/missing zscores
sum(is.na(df$Yearly_equip_failure_zScore))
#z score for Tenure 
df$Tenure_zScore <- scale(x=df$Tenure)
#identify Tenure outliers using zscore
Tenure_outliers <- df[which(df$Tenure_zScore < -3 | df$Tenure_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(Tenure_outliers) #output = 0
print(num_of_outliers)
#sort data by Tenure outliers
df_Tenure_sort <- df[order(-df$Tenure_zScore),]
#print head of df_Tenure_sort
head(df_Tenure_sort)
#print first 10 rows
df_Tenure_sort[1:10,c(1,66)]
#print first 10 rows, Tenure and churn column
df_Tenure_sort[1:10,c(42,22)]
#print first 10 rows,Tenure, techie and churn column
df_Tenure_sort[1:20,c(42,22,27)]
#check for null/missing zscores
sum(is.na(df$Tenure_zScore))
#z score for MonthlyCharge 
df$MonthlyCharge_zScore <- scale(x=df$MonthlyCharge)
#identify MonthlyCharge outliers using zscore
MonthlyCharge_outliers <- df[which(df$MonthlyCharge_zScore < -3 | df$MonthlyCharge_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(MonthlyCharge_outliers) #output = 3
print(num_of_outliers)
#sort data by MonthlyCharge outliers
df_MonthlyCharge_sort <- df[order(-df$MonthlyCharge_zScore),]
#print head of df_MonthlyCharge_sort
head(df_MonthlyCharge_sort)
#print first 3 rows
df_MonthlyCharge_sort[1:3,c(1,67)]
#print first 10 rows, MonthlyCharge and churn column
df_MonthlyCharge_sort[1:10,c(43,22)]
#print first 10 rows,MonthlyCharge, techie and churn column
df_MonthlyCharge_sort[1:20,c(43,22,27)]
#check for null/missing zscores
sum(is.na(df$MonthlyCharge_zScore))
#z score for Bandwidth_GB_Year 
df$Bandwidth_GB_Year_zScore <- scale(x=df$Bandwidth_GB_Year)
#identify Bandwidth_GB_Year outliers using zscore
Bandwidth_GB_Year_outliers <- df[which(df$Bandwidth_GB_Year_zScore < -3 | df$Bandwidth_GB_Year_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(Bandwidth_GB_Year_outliers) #output = 0
print(num_of_outliers)
#sort data by Bandwidth_GB_Year outliers
df_Bandwidth_GB_Year_sort <- df[order(-df$Bandwidth_GB_Year_zScore),]
#print head of df_Bandwidth_GB_Year_sort
head(df_Bandwidth_GB_Year_sort)
#print first 10 rows
df_Bandwidth_GB_Year_sort[1:10,c(1,68)]
#print first 10 rows, Bandwidth_GB_Year and churn column
df_Bandwidth_GB_Year_sort[1:10,c(44,22)]
#print first 10 rows,Bandwidth_GB_Year, techie and churn column
df_Bandwidth_GB_Year_sort[1:20,c(44,22,27)]
#check for null/missing zscores
sum(is.na(df$Bandwidth_GB_Year_zScore))
#z score for item1 
df$item1_zScore <- scale(x=df$item1)
#identify outliers using zscore
item1_outliers <- df[which(df$item1_zScore < -3 | df$item1_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(item1_outliers) #output = 19
print(num_of_outliers)
#sort data by item1 outliers
df_item1_sort <- df[order(-df$item1_zScore),]
#print head of df_item1_sort
head(df_item1_sort)
#print first 10 rows
df_item1_sort[1:19,c(1,69)]
#print first 10 rows, item1 and churn column
df_item1_sort[1:10,c(45,22)]
#print first 10 rows,item1, techie and churn column
df_item1_sort[1:20,c(45,22,27)]
#check for null/missing zscores
sum(is.na(df$item1_zScore))
#z score for item2 
df$item2_zScore <- scale(x=df$item2)
#identify item2 outliers using zscore
item2_outliers <- df[which(df$item2_zScore < -3 | df$item2_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(item2_outliers) #output = 13
print(num_of_outliers)
#sort data by item2 outliers
df_item2_sort <- df[order(-df$item2_zScore),]
#print head of df_item2_sort
head(df_item2_sort)
#print first 13 rows
df_item2_sort[1:13,c(1,70)]
#print first 10 rows, item2 and churn column
df_item2_sort[1:10,c(46,22)]
#print first 10 rows,item2, techie and churn column
df_item2_sort[1:20,c(46,22,27)]
#check for null/missing zscores
sum(is.na(df$item2_zScore))
#z score for item3 
df$item3_zScore <- scale(x=df$item3)
#identify item3 outliers using zscore
item3_outliers <- df[which(df$item3_zScore < -3 | df$item3_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(item3_outliers) #output = 13
print(num_of_outliers)
#sort data by item3 outliers
df_item3_sort <- df[order(-df$item3_zScore),]
#print head of df_item3_sort
head(df_item3_sort)
#print first 10 rows
df_item3_sort[1:13,c(1,71)]
#print first 10 rows, item3 and churn column
df_item3_sort[1:10,c(47,22)]
#print first 10 rows,item3, techie and churn column
df_item3_sort[1:20,c(47,22,27)]
#check for null/missing zscores
sum(is.na(df$item3_zScore))
#z score for item4 
df$item4_zScore <- scale(x=df$item4)
#identify item4 outliers using zscore
item4_outliers <- df[which(df$item4_zScore < -3 | df$item4_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(item4_outliers) #output = 9
print(num_of_outliers)
#sort data by item4 outliers
df_item4_sort <- df[order(-df$item4_zScore),]
#print head of df_item4_sort
head(df_item4_sort)
#print first 9 rows
df_item4_sort[1:9,c(1,72)]
#print first 10 rows, item4 and churn column
df_item4_sort[1:10,c(48,22)]
#print first 10 rows,item4, techie and churn column
df_item4_sort[1:20,c(48,22,27)]
#check for null/missing zscores
sum(is.na(df$item4_zScore))
#z score for item5 
df$item5_zScore <- scale(x=df$item5)
#identify item5 outliers using zscore
item5_outliers <- df[which(df$item5_zScore < -3 | df$item5_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(item5_outliers) #output = 12
print(num_of_outliers)
#sort data by item5 outliers
df_item5_sort <- df[order(-df$item5_zScore),]
#print head of df_item5_sort
head(df_item5_sort)
#print first 10 rows
df_item5_sort[1:12,c(1,73)]
#print first 10 rows, item5 and churn column
df_item5_sort[1:10,c(49,22)]
#print first 10 rows,item5, techie and churn column
df_item5_sort[1:20,c(49,22,27)]
#check for null/missing zscores
sum(is.na(df$item5_zScore))
#z score for item6 
df$item6_zScore <- scale(x=df$item6)
#identify item6 outliers using zscore
item6_outliers <- df[which(df$item6_zScore < -3 | df$item6_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(item6_outliers) #output = 13
print(num_of_outliers)
#sort data by item6 outliers
df_item6_sort <- df[order(-df$item6_zScore),]
#print head of df_item6_sort
head(df_item6_sort)
#print first 10 rows
df_item6_sort[1:13, c(1,74)]
#print first 10 rows, item6 and churn column
df_item6_sort[1:10,c(50,22)]
#print first 10 rows,item6, techie and churn column
df_item6_sort[1:20,c(50,22,27)]
#check for null/missing zscores
sum(is.na(df$item6_zScore))
#z score for item7
df$item7_zScore <- scale(x=df$item7)
#identify item7 outliers using zscore
item7_outliers <- df[which(df$item7_zScore < -3 | df$item7_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(item7_outliers) #output = 11
print(num_of_outliers)
#sort data by item7 outliers
df_item7_sort <- df[order(-df$item7_zScore),]
#print head of df_item1_sort
head(df_item7_sort)
#print first 11 rows
df_item7_sort[1:11,c(1,75)]
#print first 10 rows, item7 and churn column
df_item7_sort[1:10,c(51,22)]
#print first 10 rows,item7, techie and churn column
df_item7_sort[1:20,c(51,22,27)]
#check for null/missing zscores
sum(is.na(df$item7_zScore))
#z score for item8 
df$item8_zScore <- scale(x=df$item8)
#identify item8 outliers using zscore
item8_outliers <- df[which(df$item8_zScore < -3 | df$item8_zScore > 3),]
#calculate quantity of outliers  
num_of_outliers <- nrow(item8_outliers) #output = 15
print(num_of_outliers)
#sort data by item8 outliers
df_item8_sort <- df[order(-df$item8_zScore),]
#print head of df_item8_sort
head(df_item8_sort)
#print first 15 rows
df_item8_sort[1:15,c(1,76)]
#print first 10 rows, item8 and churn column
df_item8_sort[1:10,c(52,22)]
#print first 10 rows,item8, techie and churn column
df_item8_sort[1:20,c(52,22,27)]
#check for null/missing zscores
sum(is.na(df$item8_zScore))
#label columns in case the default labels didn’t work
colnames(df) = c("X",	"CaseOrder",	"Customer_id"	,"Interaction",	"City",	"State",	"County",	"Zip"	,"Lat",	"Lng"	,
                 "Population"	,"Area"	,"Timezone",	"Job"	,"Children"	,"Age",	"Education"	,"Employment"	,"Income"	,
                 "Marital"	,"Gender",	"Churn"	,"Outage_sec_perweek",	"Email",	"Contacts",	"Yearly_equip_failure",	
                 "Techie"	,"Contract",	"Port_modem",	"Tablet"	,"InternetService",	"Phone"	,"Multiple",	"OnlineSecurity",	
                 "OnlineBackup"	,"DeviceProtection"	,"TechSupport"	,"StreamingTV"	,"StreamingMovies"	,"PaperlessBilling",
                 "PaymentMethod",	"Tenure"	,"MonthlyCharge"	,"Bandwidth_GB_Year",	"item1"	,"item2"	,"item3"	,"item4"	,"item5"	,"item6",	"item7",	"item8",	
                 "X_zScore"	,"CaseOrder_zScore",	"Zip_zScore"	,"Lat_zScore"	,"Lng_zScore",	"Population_zScore"	,"Children_zScore"	,
                 "Age_zScore"	,"Income_zScore",	"Outage_sec_perweek_zScore",	"Email_zScore",	"Contacts_zScore"	,
                 "Yearly_equip_failure_zScore",	"Tenure_zScore",	"MonthlyCharge_zScore",	"Bandwidth_GB_Year_zScore",	"item1_zScore",
                 "item2_zScore"	,"item3_zScore"	,"item4_zScore"	,"item5_zScore"	,
                 "item6_zScore"	,"item7_zScore"	,"item8_zScore") 

#install corrplot so we can check correlations between churn and variables
#library(corrplot)
#install.packages("corrplot")
#create corrplot of all numeric variables in data frame
#df_numeric <- df[sapply(df, is.numeric)]
# Add the Churns variable
#df_numeric$Churns <- as.numeric(as.factor(df$Churn)) 
#create cor table
#cor.table <- cor(df_numeric)
#create corrplot
#corrplot(cor.table, method = "color", tl.cex = 0.6, tl.srt = 45)
#histogram of Age
hist(df$Age,xlab="age", main = "Histogram of age")
#histogram of Income
hist(df$Income, xlab="Income", main= "Histogram on Income")
#histogram for Children
hist(df$Children,xlab="children", main = "Histogram of children")
#histogram for Outage_sec_perweek
hist(df$Outage_sec_perweek, xlab="Outage_sec_perweek", main = "Histogram of Outage_sec_perweek")
#histogram for Techie (1 is no 2 is yes) convert to numeric from char
df_numeric$Techie <- as.numeric(as.factor(df$Techie)) 
hist(df_numeric$Techie, xlab="Techie", main = "Histogram of Techie")
#histogram for Phone convert to numeric (1 is no 2 is yes)
df_numeric$Phone <- as.numeric(as.factor(df$Phone)) 
hist(df_numeric$Phone, xlab="Phone", main = "Histogram of Phone")
#histogram for TechSupport convert to numeric (1 is no 2 is yes)
df_numeric$TechSupport <- as.numeric(as.factor(df$TechSupport)) 
hist(df_numeric$TechSupport, xlab="TechSupport", main = "Histogram of TechSupport")
#histogram for Bandwidth_GB_Year
hist(df$Bandwidth_GB_Year, xlab="Bandwidth_GB_Year", main = "Histogram of Bandwidth_GB_Year")
#histogram for Tenure
hist(df$Tenure, xlab="Tenure", main = "Histogram of Tenure")


#replace NA values in col Age - avg

df$Age[is.na(df$Age)] <- round(mean(df$Age, na.rm=TRUE))
#confirm replace age NA with mean
sum(is.na(df$Age))
#replace NA Values in col Income- AVG negative skew normal distribution
df$Income[is.na(df$Income)] <- mean(df$Income, na.rm=TRUE)
sum(is.na(df$Income))
#replace NA values in col Children - normal distribution will use mode "0"
df[,"Children"]<- as.numeric(df[,"Children"])
df$Children[is.na(df$Children)] <- 0
#confirm replace Children NA with mode 
sum(is.na(df$Children))
#replace NA values in col Outage_sec_perweek - negative skew bimodal distribution will use mode
df$Outage_sec_perweek[is.na(df$Outage_sec_perweek)] <- mode(df$Outage_sec_perweek)
#confirm replace Outage_sec_perweek NA with mode 
sum(is.na(df$Outage_sec_perweek))
#replace NA values in col Techie - negative skew distribution will use mode "1" for No. hardcode value since using mode resulted in new column added to histogram. 
#converting to numeric here to impute values - otherwise major errors occurred with data type inconsistancies
df[,"Techie"]<- as.numeric(df[,"Techie"])
df$Techie[is.na(df$Techie)] <- 1
#confirm replace Techie NA with mode 
sum(is.na(df$Techie))
#replace NA values in col Phone - positive skew distribution will use mode. Hardcoded value since using mode resulted in new column added to histogram. 
#converting to numeric here to impute values - otherwise major errors occurred with data type inconsistencies
df[,"Phone"]<- as.numeric(df[,"Phone"])
df$Phone[is.na(df$Phone)] <- 2
#confirm replace Phone NA with mode 
sum(is.na(df$Phone))
#replace NA values in col TechSupport - negative skew distribution will use mode. Hardcoded value since using mode resulted in new column added to histogram. 
#converting to numeric here to impute values - otherwise major errors occurred with data type inconsistencies
df[,"TechSupport"]<- as.numeric(df[,"TechSupport"])
df$TechSupport[is.na(df$TechSupport)] <- 1
#confirm replace TechSupport NA with mode 
sum(is.na(df$TechSupport))
#replace NA values in col Bandwidth_GB_Year - has bimodal distribution- will use average
df$Bandwidth_GB_Year[is.na(df$Bandwidth_GB_Year)] <- mean(df$Bandwidth_GB_Year, na.rm=TRUE)
#confirm replace Bandwidth_GB_Year NA with mean
sum(is.na(df$Bandwidth_GB_Year))
#replace NA values in col Tenure -has bimodal distribution- will use average
df$Tenure[is.na(df$Tenure)] <- mean(df$Tenure , na.rm=TRUE)
#confirm replace Tenure NA with mean
sum(is.na(df$Tenure))

# recheck histograms after changes made
#histogram of Age
hist(df$Age,xlab="age", main = "Histogram of age")
#histogram of Income
hist(df$Income, xlab="Income", main= "Histogram on Income")
#histogram for Children
#update datatype as error thrown for data type issue- needed to be numeric
hist(df$Children,xlab="children", main = "Histogram of children")
#histogram for Outage_sec_perweek
#update datatype as error thrown for data type issue- needed to be numeric
df$Outage_sec_perweek <- as.numeric(as.character(df$Outage_sec_perweek))
hist(df$Outage_sec_perweek, xlab="Outage_sec_perweek", main = "Histogram of Outage_sec_perweek")
#histogram for Techie (1 is no 2 is yes) convert to numeric from char
hist(df_numeric$Techie, xlab="Techie", main = "Histogram of Techie")
#histogram for Phone convert to numeric (1 is no 2 is yes)
hist(df_numeric$Phone, xlab="Phone", main = "Histogram of Phone")
#histogram for TechSupport convert to numeric (1 is no 2 is yes)
#df_numeric$TechSupport <- as.numeric(as.factor(df$TechSupport)) 
hist(df_numeric$TechSupport, xlab="TechSupport", main = "Histogram of TechSupport")
#histogram for Bandwidth_GB_Year
hist(df$Bandwidth_GB_Year, xlab="Bandwidth_GB_Year", main = "Histogram of Bandwidth_GB_Year")
#histogram for Tenure
hist(df$Tenure, xlab="Tenure", main = "Histogram of Tenure")

# setting data types per variable ensure correct data types in each column
df[,"X"]<- as.integer(df[,"X"])
df[,"CaseOrder"]<- as.integer(df[,"CaseOrder"])
df[,"Customer_id"]<- as.character(df[,"Customer_id"])
df[,"Interaction"]<- as.character(df[,"Interaction"])
df[,"City"]<- as.character(df[,"City"])
df[,"State"]<- as.character(df[,"State"])
df[,"County"]<- as.character(df[,"County"])
df[,"Zip"]<- as.integer(df[,"Zip"])
df[,"Lat"]<- as.numeric(df[,"Lat"])
df[,"Lng"]<- as.numeric(df[,"Lng"])
df[,"Population"]<- as.integer(df[,"Population"])
df[,"Area"]<- as.character(df[,"Area"])
df[,"Timezone"]<- as.character(df[,"Timezone"])
df[,"Job"]<- as.character(df[,"Job"])
df[,"Children"]<- as.integer(df[,"Children"])
df[,"Age"]<- as.integer(df[,"Age"])
df[,"Education"]<- as.character(df[,"Education"])
df[,"Employment"]<- as.character(df[,"Employment"])
df[,"Income"]<- as.numeric(df[,"Income"])
df[,"Marital"]<- as.character(df[,"Marital"])
df[,"Gender"]<- as.character(df[,"Gender"])
df[,"Churn"]<- as.character(df[,"Churn"])
df[,"Outage_sec_perweek"]<- as.numeric(df[,"Outage_sec_perweek"])
df[,"Email"]<- as.integer(df[,"Email"])
df[,"Contacts"]<- as.integer(df[,"Contacts"])
df[,"Yearly_equip_failure"]<- as.integer(df[,"Yearly_equip_failure"])
df[,"Techie"]<- as.character(df[,"Techie"])
df[,"Contract"]<- as.character(df[,"Contract"])
df[,"Port_modem"]<- as.character(df[,"Port_modem"])
df[,"Tablet"]<- as.character(df[,"Tablet"])
df[,"InternetService"]<- as.character(df[,"InternetService"])
df[,"Phone"]<- as.character(df[,"Phone"])
df[,"Multiple"]<- as.character(df[,"Multiple"])
df[,"OnlineSecurity"]<- as.character(df[,"OnlineSecurity"])
df[,"OnlineBackup"]<- as.character(df[,"OnlineBackup"])
df[,"DeviceProtection"]<- as.character(df[,"DeviceProtection"])
df[,"TechSupport"]<- as.character(df[,"TechSupport"])
df[,"StreamingTV"]<- as.character(df[,"StreamingTV"])
df[,"StreamingMovies"]<- as.character(df[,"StreamingMovies"])
df[,"PaperlessBilling"]<- as.character(df[,"PaperlessBilling"])
df[,"PaymentMethod"]<- as.character(df[,"PaymentMethod"])
df[,"Tenure"]<- as.numeric(df[,"Tenure"])
df[,"MonthlyCharge"]<- as.numeric(df[,"MonthlyCharge"])
df[,"Bandwidth_GB_Year"]<- as.numeric(df[,"Bandwidth_GB_Year"])
df[,"item1"]<- as.integer(df[,"item1"])
df[,"item2"]<- as.integer(df[,"item2"])
df[,"item3"]<- as.integer(df[,"item3"])
df[,"item4"]<- as.integer(df[,"item4"])
df[,"item5"]<- as.integer(df[,"item5"])
df[,"item6"]<- as.integer(df[,"item6"])
df[,"item7"]<- as.integer(df[,"item7"])
df[,"item8"]<- as.integer(df[,"item8"])
df[,"X_zScore"] <- as.numeric(df[,"X_zScore"])
df[,"CaseOrder_zScore"] <- as.numeric(df[,"CaseOrder_zScore"])
df[,"Zip_zScore"] <- as.numeric(df[,"Zip_zScore"])
df[,"Lat_zScore"] <- as.numeric(df[,"Lat_zScore"])
df[,"Lng_zScore"] <- as.numeric(df[,"Lng_zScore"])
df[,"Population_zScore"] <- as.numeric(df[,"Population_zScore"])
df[,"Children_zScore"] <- as.numeric(df[,"Children_zScore"])
df[,"Age_zScore"] <- as.numeric(df[,"Age_zScore"])
df[,"Income_zScore"] <- as.numeric(df[,"Income_zScore"])
df[,"Outage_sec_perweek_zScore"] <- as.numeric(df[,"Outage_sec_perweek_zScore"])
df[,"Email_zScore"] <- as.numeric(df[,"Email_zScore"])
df[,"Contacts_zScore"] <- as.numeric(df[,"Contacts_zScore"])
df[,"Yearly_equip_failure_zScore"] <- as.numeric(df[,"Yearly_equip_failure_zScore"])
df[,"Tenure_zScore"] <- as.numeric(df[,"Tenure_zScore"])
df[,"MonthlyCharge_zScore"] <- as.numeric(df[,"MonthlyCharge_zScore"])
df[,"Bandwidth_GB_Year_zScore"] <- as.numeric(df[,"Bandwidth_GB_Year_zScore"])
df[,"item1_zScore"] <- as.numeric(df[,"item1_zScore"])
df[,"item2_zScore"] <- as.numeric(df[,"item2_zScore"])
df[,"item3_zScore"] <- as.numeric(df[,"item3_zScore"])
df[,"item4_zScore"] <- as.numeric(df[,"item4_zScore"])
df[,"item5_zScore"] <- as.numeric(df[,"item5_zScore"])
df[,"item6_zScore"] <- as.numeric(df[,"item6_zScore"])
df[,"item7_zScore"] <- as.numeric(df[,"item7_zScore"])
df[,"item8_zScore"] <- as.numeric(df[,"item8_zScore"])


write.csv(df, "churn_data_clean.csv", row.names = FALSE)
# use dplyr to drop col Age_zScore due to error
#df <-select(df, -Age_zScore)
summary(df)
#initialize data for pca analysis
# initialize variables y,X,X_test (X and X_test split df in half)
#columns/variables used are quantitative and continuous
y <- df$Churn
X <- df[1:5000,c(9,10, 19, 23, 42:44)]
X_test <- df[5001:10000,c(9,10, 19, 23, 42:44)]

#standardize predictor vars
X_z <- as.data.frame(scale(X))
colnames(X_z) <- c("Lat", "Lng", "Income", "Outage_sec_perweek", "Tenure", "MonthlyCharge", "Bandwidth_GB_Year")
round(cor(X_z), 3)
#install.packages("psych")
library(psych)
#conduct pca- result K=6
pca1 <- principal(r=X_z, rotate ="varimax", nfactors=7)
##component weights
print(pca1$loadings, cutoff= .49)
#convert ss load results to vectors to plot
ss_load <- c(1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 0.999)
#eigenvalues screeplot
plot(ss_load, type="b", main = "Plot of Eigenvalues (scree plot)", ylab = "Value", xlab= "Component"); abline(h=1, lty=2)

#without rotation- result K=4 
pca2_no_rotation <-principal(r=X, rotate="none", nfactors=6)
#component weights
print(pca2_no_rotation$loadings,cutoff=.5)

#with rotation- result K=5 
pca2_rotation<-principal(r=X, rotate="varimax", nfactors=6)
#component weights
print(pca2_rotation$loadings,cutoff=.5)

#confirm pca results
X_test_z <- scale(X_test)
#confirm 6 PCA components recommended K=6
pca2_test <- principal(r=X_test_z, rotate="varimax", nfactors=7)
#component weights
pca2_test$loadings
#confirmed 6 components (K=6)
#correlation of components in training set
#shows components not correlated below
round(cor(pca2_rotation$scores), 2)



