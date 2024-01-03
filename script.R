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
