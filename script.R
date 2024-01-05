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


#z score for age
#df$Age_zScore <- scale(x=df$Age)
#identify outliers
#age_outliers <- df[which(df$Age_zScore < -3 | df$Age_zScore > 3),]
#sort data by age outliers
#df_age_sort <- df[order(-df$Age_zScore),]
#print head of df_age_sort
#head(df_age_sort)
#print first 10 rows
#df_age_sort[1:10,]
#print first 10 rows, age and churn column
#df_age_sort[1:10,c(16,22)]
#print first 10 rows,age, techie and churn column
#df_age_sort[1:20,c(16,22,27)]
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
# use dplyr to drop col Age_zScore due to error- also not necessary
#df <-select(df, -Age_zScore)
summary(df)
#initialize data for pca analysis
selected_columns <- c(2, 8:11, 15, 16, 19, 23:27, 32, 37, 42:52)
df_subset <- df[, selected_columns]
summary(df_subset)
df.pca <- prcomp(df_subset, center= TRUE, scale=TRUE)
summary(df.pca)
install.packages("factoextra")
library(factoextra)
#scree plot of eigenvalues to indicate number of PCs in the PCA "Principal Component Analysis"
fviz_eig(df.pca, choice = "eigenvalue", addlabels=TRUE)
#summary of pca output
summary(df.pca)

