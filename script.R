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


#z score for X (row identifier) 
df$X_zScore <- scale(x=df$X)
#identify X outliers using zscore
X_outliers <- df[which(df$X_zScore < -3 | df$X_zScore > 3),]
#sort data by X outliers
df_X_sort <- df[order(-df$X_zScore),]
#print head of df_X_sort
head(df_X_sort)
#print first 10 rows
df_X_sort[1:10,]
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
#sort data by CaseOrder outliers
df_CaseOrder_sort <- df[order(-df$CaseOrder_zScore),]
#print head of df_age_sort
head(df_CaseOrder_sort)
#print first 10 rows
df_CaseOrder_sort[1:10,]
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
#sort data by Zip outliers
df_Zip_sort <- df[order(-df$Zip_zScore),]
#print head of df_Zip_sort
head(df_Zip_sort)
#print first 10 rows
df_Zip_sort[1:10,]
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
#sort data by Lat outliers
df_Lat_sort <- df[order(-df$Lat_zScore),]
#print head of df_Lat_sort
head(df_Lat_sort)
#print first 10 rows
df_Lat_sort[1:10,]
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
#sort data by Lng outliers
df_Lng_sort <- df[order(-df$Lng_zScore),]
#print head of df_Lng_sort
head(df_Lng_sort)
#print first 10 rows
df_Lng_sort[1:10,]
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
#sort data by Population outliers
df_Population_sort <- df[order(-df$Population_zScore),]
#print head of df_Population_sort
head(df_Population_sort)
#print first 10 rows
df_Population_sort[1:10,]
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
#sort data by age outliers
df_Children_sort <- df[order(-df$Children_zScore),]
#print head of df_Children_sort
head(df_Children_sort)
#print first 10 rows
df_Children_sort[1:10,]
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
#sort data by age outliers
df_age_sort <- df[order(-df$Age_zScore),]
#print head of df_age_sort
head(df_age_sort)
#print first 10 rows
df_age_sort[1:10,]
#print first 10 rows, age and churn column
df_age_sort[1:10,c(16,22)]
#print first 10 rows,age, techie and churn column
df_age_sort[1:20,c(16,22,27)]
#check for null/missing zscores
sum(is.na(df$Age_zScore))

#z score for age 
df$Age_zScore <- scale(x=df$Age)
#identify outliers using zscore
age_outliers <- df[which(df$Age_zScore < -3 | df$Age_zScore > 3),]
#sort data by age outliers
df_age_sort <- df[order(-df$Age_zScore),]
#print head of df_age_sort
head(df_age_sort)
#print first 10 rows
df_age_sort[1:10,]
#print first 10 rows, age and churn column
df_age_sort[1:10,c(16,22)]
#print first 10 rows,age, techie and churn column
df_age_sort[1:20,c(16,22,27)]
#check for null/missing zscores
sum(is.na(df$Age_zScore))

#z score for Income 
df$Income_zScore <- scale(x=df$Income)
#identify Income outliers using zscore
Income_outliers <- df[which(df$Income_zScore < -3 | df$Income_zScore > 3),]
#sort data by age outliers
df_Income_sort <- df[order(-df$Income_zScore),]
#print head of df_Income_sort
head(df_Income_sort)
#print first 10 rows
df_Income_sort[1:10,]
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
#sort data by Outage_sec_perweek outliers
df_Outage_sec_perweek_sort <- df[order(-df$Outage_sec_perweek_zScore),]
#print head of df_Outage_sec_perweek_sort
head(df_Outage_sec_perweek_sort)
#print first 10 rows
df_age_sort[1:10,]
#print first 10 rows, Outage_sec_perweek and churn column
df_age_sort[1:10,c(23,22)]
#print first 10 rows,Outage_sec_perweek, techie and churn column
df_age_sort[1:20,c(23,22,27)]
#check for null/missing zscores
sum(is.na(df$Outage_sec_perweek_zScore))

#z score for Email 
df$Email_zScore <- scale(x=df$Email)
#identify Email outliers using zscore
Email_outliers <- df[which(df$Email_zScore < -3 | df$Email_zScore > 3),]
#sort data by Email outliers
df_Email_sort <- df[order(-df$Email_zScore),]
#print head of df_Email_sort
head(df_Email_sort)
#print first 10 rows
df_Email_sort[1:10,]
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
#sort data by Contacts outliers
df_Contacts_sort <- df[order(-df$Contacts_zScore),]
#print head of df_Contacts_sort
head(df_Contacts_sort)
#print first 10 rows
df_Contacts_sort[1:10,]
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
#sort data by Yearly_equip_failure outliers
df_Yearly_equip_failure_sort <- df[order(-df$Yearly_equip_failure_zScore),]
#print head of df_Yearly_equip_failure_sort
head(df_Yearly_equip_failure_sort)
#print first 10 rows
df_age_sort[1:10,]
#print first 10 rows, Yearly_equip_failure and churn column
df_age_sort[1:10,c(26,22)]
#print first 10 rows,Yearly_equip_failure, techie and churn column
df_age_sort[1:20,c(26,22,27)]
#check for null/missing zscores
sum(is.na(df$Yearly_equip_failure_zScore))

#z score for Tenure 
df$Tenure_zScore <- scale(x=df$Tenure)
#identify Tenure outliers using zscore
Tenure_outliers <- df[which(df$Tenure_zScore < -3 | df$Tenure_zScore > 3),]
#sort data by Tenure outliers
df_Tenure_sort <- df[order(-df$Tenure_zScore),]
#print head of df_Tenure_sort
head(df_Tenure_sort)
#print first 10 rows
df_Tenure_sort[1:10,]
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
#sort data by MonthlyCharge outliers
df_MonthlyCharge_sort <- df[order(-df$MonthlyCharge_zScore),]
#print head of df_MonthlyCharge_sort
head(df_MonthlyCharge_sort)
#print first 10 rows
df_MonthlyCharge_sort[1:10,]
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
#sort data by Bandwidth_GB_Year outliers
df_Bandwidth_GB_Year_sort <- df[order(-df$Bandwidth_GB_Year_zScore),]
#print head of df_Bandwidth_GB_Year_sort
head(df_Bandwidth_GB_Year_sort)
#print first 10 rows
df_age_sort[1:10,]
#print first 10 rows, Bandwidth_GB_Year and churn column
df_age_sort[1:10,c(44,22)]
#print first 10 rows,Bandwidth_GB_Year, techie and churn column
df_age_sort[1:20,c(44,22,27)]
#check for null/missing zscores
sum(is.na(df$Bandwidth_GB_Year_zScore))

#z score for item1 
df$item1_zScore <- scale(x=df$item1)
#identify outliers using zscore
item1_outliers <- df[which(df$item1_zScore < -3 | df$item1_zScore > 3),]
#sort data by item1 outliers
df_item1_sort <- df[order(-df$item1_zScore),]
#print head of df_item1_sort
head(df_item1_sort)
#print first 10 rows
df_item1_sort[1:10,]
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
#sort data by item2 outliers
df_item2_sort <- df[order(-df$item2_zScore),]
#print head of df_item2_sort
head(df_item2_sort)
#print first 10 rows
df_item2_sort[1:10,]
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
#sort data by item3 outliers
df_item3_sort <- df[order(-df$item3_zScore),]
#print head of df_item3_sort
head(df_item3_sort)
#print first 10 rows
df_item3_sort[1:10,]
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
#sort data by item4 outliers
df_item4_sort <- df[order(-df$item4_zScore),]
#print head of df_item4_sort
head(df_item4_sort)
#print first 10 rows
df_item4_sort[1:10,]
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
#sort data by item5 outliers
df_item5_sort <- df[order(-df$item5_zScore),]
#print head of df_item5_sort
head(df_item5_sort)
#print first 10 rows
df_item5_sort[1:10,]
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
#sort data by item6 outliers
df_item6_sort <- df[order(-df$item6_zScore),]
#print head of df_item6_sort
head(df_item6_sort)
#print first 10 rows
df_item6_sort[1:10,]
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
#sort data by item7 outliers
df_item7_sort <- df[order(-df$item7_zScore),]
#print head of df_item1_sort
head(df_item7_sort)
#print first 10 rows
df_item7_sort[1:10,]
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
#sort data by item8 outliers
df_item8_sort <- df[order(-df$item8_zScore),]
#print head of df_item8_sort
head(df_item8_sort)
#print first 10 rows
df_item8_sort[1:10,]
#print first 10 rows, item8 and churn column
df_item8_sort[1:10,c(52,22)]
#print first 10 rows,item8, techie and churn column
df_item8_sort[1:20,c(52,22,27)]
#check for null/missing zscores
sum(is.na(df$item8_zScore))
na_counts <- colSums(is.na(df))
print(na_counts)


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

df$Age[is.na(df$Age)] <- mean(df$Age, na.rm=TRUE)
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

write.csv(df, "churn_data_clean.csv", row.names = FALSE)
# use dplyr to drop col Age_zScore due to error
#df <-select(df, -Age_zScore)
summary(df)
#initialize data for pca analysis
selected_columns <- c(1,2, 8:11, 15, 16, 19, 23:27, 32, 37, 42:52)
df_subset <- df[, selected_columns]
summary(df_subset)
#due to zero column variance- will implement sort to remove columns
zero_variance_cols <- apply(df_subset, 2, var) == 0
df_subset_zero <- df_subset[, !zero_variance_cols]
df.pca <- prcomp(df_subset_zero, center= TRUE, scale=TRUE)
summary(df.pca)
#install.packages("factoextra")
library(factoextra)
#scree plot of eigenvalues to indicate number of PCs in the PCA "Principal Component Analysis"
fviz_eig(df.pca, choice = "eigenvalue", addlabels=TRUE)
#summary of pca output
summary(df.pca)

