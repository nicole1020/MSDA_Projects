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
#histogram of Age
hist(df$Age,xlab="age", main = "histogram of age")
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
#z score for age
df$Age_zScore <- scale(x=df$Age)
#identify outliers
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
#install corrplot so we can check correlations between churn and variables
library(corrplot)
#install.packages("corrplot")
df_numeric <- df[sapply(df, is.numeric)]  # Select only numeric columns
df_numeric$Churns <- as.numeric(as.factor(df$Churn))  # Add the Churns variable
cor.table <- cor(df_numeric)
corrplot(cor.table)
