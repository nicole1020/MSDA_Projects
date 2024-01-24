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
#check for duplicate data
df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
#install ggplot2 for data visualization
#install.packages("ggplot2")
library(ggplot2)
#install dplyr for data manipulation
#install.packages("dplyr")
library(dplyr)

#calculate outliers for all quantitative variables
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
summary(df_Lat_sort)
#print first 76 rows and last 76 rows in col 53 for Lat_zScore
df_Lat_sort[1:76,c(1,53)]
df_Lat_sort[9926:10000,c(1,53)]
#print first 10 rows, Lat and churn column
df_Lat_sort[1:10,c(9,53)]
#print first 10 rows,Lat, techie and churn column
df_Lat_sort[1:20,c(9,22,53)]
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
#print first and last 102 rows in col 54 for Lng_zScore
df_Lng_sort[9899:10000,c(1,54)]
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
df_Population_sort[1:219,c(1,55)]
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
num_of_outliers <- nrow(Children_outliers) #output = 144
print(num_of_outliers)
#sort data by Children outliers
df_Children_sort <- df[order(-df$Children_zScore),]
#print head of df_Children_sort
head(df_Children_sort)
#print first 10 rows 144 col 56
df_Children_sort[1:144,c(1,56)]
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
df_age_sort[1:10,c(1,57)]
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
num_of_outliers <- nrow(Income_outliers) #output =  110
print(num_of_outliers)
#sort data by age outliers
df_Income_sort <- df[order(-df$Income_zScore),]
#print head of df_Income_sort
head(df_Income_sort)
#print first 180
df_Income_sort[1:110,c(1,58)]
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
df_Outage_sec_perweek_sort[1:491,c(1,59)]
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
#print first 3 rows and last 9 rows with outliers
df_Email_sort[1:3,c(1,60)]
df_Email_sort[9992:10000,60]
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
df_Contacts_sort[1:165,c(1,61)]
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
df_Yearly_equip_failure_sort[1:94,c(1,62)]
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
df_Tenure_sort[1:10,c(1,63)]
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
df_MonthlyCharge_sort[1:3,c(1,64)]
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
df_Bandwidth_GB_Year_sort[1:10,c(1,65)]
#print first 10 rows, Bandwidth_GB_Year and churn column
df_Bandwidth_GB_Year_sort[1:10,c(44,22)]
#print first 10 rows,Bandwidth_GB_Year, techie and churn column
df_Bandwidth_GB_Year_sort[1:20,c(44,22,27)]
#check for null/missing zscores
sum(is.na(df$Bandwidth_GB_Year_zScore))

#Histogram for all variables with missing values
#histogram of Age
hist(df$Age,xlab="age", main = "Histogram of age")
#histogram of Income
hist(df$Income, xlab="Income", main= "Histogram on Income")
#histogram for Children
hist(df$Children,xlab="children", main = "Histogram of children")
#histogram for Techie (1 is no 2 is yes) convert to numeric from char
df$Techie <- as.numeric(as.factor(df$Techie)) 
hist(df$Techie, xlab="Techie", main = "Histogram of Techie")
#histogram for Phone convert to numeric (1 is no 2 is yes)
df$Phone <- as.numeric(as.factor(df$Phone)) 
hist(df$Phone, xlab="Phone", main = "Histogram of Phone")
#histogram for TechSupport convert to numeric (1 is no 2 is yes)
df$TechSupport <- as.numeric(as.factor(df$TechSupport)) 
hist(df$TechSupport, xlab="TechSupport", main = "Histogram of TechSupport")
#histogram for Bandwidth_GB_Year
hist(df$Bandwidth_GB_Year, xlab="Bandwidth_GB_Year", main = "Histogram of Bandwidth_GB_Year")
#histogram for Tenure
hist(df$Tenure, xlab="Tenure", main = "Histogram of Tenure")

#impute and replace NA values in all columns with NA values
#impute age- numeric not skewed- impute mean
df$Age[is.na(df$Age)] <- round(mean(df$Age, na.rm=TRUE))
#confirm replace age NA with mean
sum(is.na(df$Age))
#replace NA Values in col Income- AVG negative skew normal distribution with skew and numeric impute median
df$Income[is.na(df$Income)] <- median(df$Income, na.rm=TRUE)
#confirm replace Income NA with median 
sum(is.na(df$Income))
#replace NA values in col Children - distribution negative skew and numeric data  will use median 
df$Children[is.na(df$Children)] <- median(df$Children, na.rm=TRUE)
#confirm replace Children NA with median 
sum(is.na(df$Children))
#replace NA values in col Techie - negative skew distribution, but categorical string variable will use mode "1" for No. hardcode value since using mode resulted in new column added to histogram. 
#converting to numeric here to impute values - otherwise major errors occurred with data type inconsistancies
df[,"Techie"]<- as.numeric(df[,"Techie"])
df$Techie[is.na(df$Techie)] <- 1
#confirm replace Techie NA with mode 
sum(is.na(df$Techie))
#replace NA values in col Phone - positive skew distribution but categorical string variable will use mode. Hardcoded value since using mode resulted in new column added to histogram. 
#converting to numeric here to impute values - otherwise major errors occurred with data type inconsistencies
df[,"Phone"]<- as.numeric(df[,"Phone"])
df$Phone[is.na(df$Phone)] <- 2
#confirm replace Phone NA with mode 
sum(is.na(df$Phone))
#replace NA values in col TechSupport - negative skew distribution but categorical string variable will use mode. Hardcoded value since using mode resulted in new column added to histogram. 
#converting to numeric here to impute values - otherwise major errors occurred with data type inconsistencies
df[,"TechSupport"]<- as.numeric(df[,"TechSupport"])
df$TechSupport[is.na(df$TechSupport)] <- 1
#confirm replace TechSupport NA with mode 
sum(is.na(df$TechSupport))
#replace NA values in col Tenure -has bimodal distribution- will use median
df$Tenure[is.na(df$Tenure)] <- median(df$Tenure , na.rm=TRUE)
#confirm replace Tenure NA with median
sum(is.na(df$Tenure))
#replace NA values in col Bandwidth_GB_Year - has bimodal distribution- will use median
df$Bandwidth_GB_Year[is.na(df$Bandwidth_GB_Year)] <- median(df$Bandwidth_GB_Year, na.rm=TRUE)
#confirm replace Bandwidth_GB_Year NA with mean
sum(is.na(df$Bandwidth_GB_Year))

#check for null values
colSums(is.na(df))

# recheck histograms to confirm imputed NA values after changes made
#histogram of Age
hist(df$Age,xlab="age", main = "Histogram of age")
#histogram of Income
hist(df$Income, xlab="Income", main= "Histogram on Income")
#histogram for Children
hist(df$Children,xlab="children", main = "Histogram of children")
#histogram for Techie (1 is no 2 is yes) convert to numeric from char
hist(df$Techie, xlab="Techie", main = "Histogram of Techie")
#histogram for Phone convert to numeric (1 is no 2 is yes)
hist(df$Phone, xlab="Phone", main = "Histogram of Phone")
#histogram for TechSupport convert to numeric (1 is no 2 is yes)
hist(df$TechSupport, xlab="TechSupport", main = "Histogram of TechSupport")
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
write.csv(df, "churn_data_clean.csv", row.names = FALSE)

summary(df)

#initialize data for pca analysis
#select rows and specific columns and run pca
df.pca <- prcomp(df[,c(9,10, 19, 23, 42:44)], center=TRUE, scale=TRUE)
#summary of pca output
summary(df.pca)
#install.packages("factoextra")
library(factoextra)

#loadings matrix
df.pca$rotation

#scree plot of eigenvalues to indicate number of PCs in the PCA "Principal Component Analysis"
fviz_eig(df.pca, choice = "eigenvalue", addlabels=TRUE)


