suppressMessages(library(tidyverse)) 
suppressMessages(library(purrr)) 
suppressMessages(library(lubridate)) 
suppressMessages(library(funModeling))
suppressMessages(library(ggplot2))
suppressMessages(library(DataExplorer))
suppressMessages(library(data.table))
suppressMessages(library(Hmisc))
#install.packages("pastecs")
library(pastecs)
#install.packages("psych")
library(psych)

install.packages(c("gridExtra", 
                   "knitr",
                   "lubridate", 
                   "rmarkdown", 
                   "tidyr", 
                   "dplyr",
                   "ggplot2",
                   "recommenderlab",
                   "reshape2",
                   "cluster", 
                   "lubridate"))

library(gridExtra)
library(knitr)
library(lubridate)
library(rmarkdown)
library(tidyr)
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(reshape2)
library(cluster)
library(lubridate)

#stat.desc(data)

#Function for Mode from Jai's code
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Read the data from CSV
data <- read.csv("data.csv")

#Structure of the Data
# 6 Factors (category variables), Date is listed as a factor, need to be converted into a date data type
# 2 numeric (continous variables)
str(data)

#Top 5 rows of the data
head(data)

# From HMISC library for descriptive statistics
# Hmisc::describe(data)

# From psych library for descriptive statistics
psych::describe(data)

#df_status function from DataExplorer package provides the following information regarding missing, zero and infinite values.
df_status(data)
#plot_missing(data)

#map function from purrr package to get variable wise number of NA's
#map_df(data, function(x) sum(is.na(x)))

# Total number of missing values from the data set. Customer IDs = 135080
apply(data, 2, function(x){sum(is.na(x))})
paste('Number of Missing Values:', sum(is.na(data)))

# data1 <- data
# Remove missing values from the data
# data1 <- data1 %>% na.omit()
# df_status(data1)
# glimpse(data)
# glimpse(data1)


# Add Cancelled Column to ID cancelled orders

Cancelled <- ifelse(substr(data$InvoiceNo,1,1)=="C","Y","N")
data<-cbind(data,Cancelled)
prop.table(table(Cancelled))

# From Marketing Project Code Samples. Split data in to Complete and Missing Data sets
missing_data <- data[is.na(data$CustomerID),] # THis data set contains all the rows where customer ID is missing
complete_data <- data[!is.na(data$CustomerID),] # This dataset contains all the rows where customer ID is not missing


# From https://www.kaggle.com/chinarxo/e-commerce-eda-clustering

data$InvoiceDate<- as.character(data$InvoiceDate)

data$Date <- sapply(data$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
data$Time <- sapply(data$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})
data$Month <- sapply(data$Date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][1]})
data$Year <- sapply(data$Date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][3]})

data$CustomerID <- as.factor(data$CustomerID)
data$Date <- as.Date(data$Date, '%m/%d/%Y')
# data$dayOfWeek <- wday(data$Date, label=TRUE)
# str(data)
data$Year <- as.factor(data$Year)
data$Month <- as.factor(data$Month)

data$Month <- month.abb[data$Month]
data$Month <- as.factor(data$Month)
###########################################
# Variable 1 - Invoice Number (Categorical)
##########################################
# Total number of unique invoices = 25900
length(unique(data$InvoiceNo))
# Total number of unique invoices in Complete Data
length(unique(complete_data$InvoiceNo))
# Univariate Analysis for Invoice Number
#frequency table for Invoice Number
table(data$InvoiceNo)
# histogram for country 
hist(table(data$InvoiceNo))


###########################################
# Variable 2 - Stock Code (Categorical)
##########################################
 
# Total number of unique stock codes 4070
length(unique(data$StockCode))
# Total number of unique stock codes in Complete Data 3684
length(unique(complete_data$StockCode))
table(data$StockCode)
# histogram for StockCode
hist(table(data$StockCode))


###########################################
# Variable 3 - Description (Categorical)
##########################################
# Total number of unique stock descriptions 4224
length(unique(data$Description))
# Total number of unique stock descriptions in Complete Data 3896
length(unique(complete_data$Description))
table(data$Description)
# histogram for Description
hist(table(data$Description))

###########################################
# Variable 4 - Quantity (Continuous)
##########################################
# Central Tendencies and Dispersion Measures for Quantity
psych::describe(data$Quantity)
summary(data$Quantity)
var(data$Quantity)
sd(data$Quantity)
stat.desc(data$Quantity)

#Plot in 2x2 Grid
par(mfrow=c(2,2))
#Box Plots for Continuous Variables
boxplot(data$Quantity, main ="Boxplot- Qty")
glimpse(data)
# Remove extreme outliers in quantity
# complete_data1 <- filter(complete_data, Quantity!=c(-80995.00,80995.00))
# glimpse(complete_data1)
# boxplot(complete_data1$Quantity, main ="Boxplot- Qty")
hist(data$Quantity)
hist(data$Quantity[which(abs(data$Quantity)<100)])
#Remove Outliers?
#Remove negative values?

###########################################
# Variable 5 - InvoiceDate (Continuous)
##########################################
# Total number of unique InvoiceDate values 23260
length(unique(data$InvoiceDate))
# Total number of unique InvoiceDate values in Complete Data 20460
#length(unique(complete_data$InvoiceDate))
table(data$InvoiceDate)
# histogram for Description
hist(table(data$InvoiceDate))

#InvoiceDate Feature Engineering


###########################################
# Variable 6 - UnitPrice (Continuous)
##########################################
psych::describe(data$UnitPrice)
summary(data$UnitPrice)
var(data$UnitPrice)
sd(data$UnitPrice)
# stat.desc(data$UnitPrice)
# psych::describe(complete_data$UnitPrice)
# summary(complete_data$UnitPrice)
# var(complete_data$UnitPrice)
# sd(complete_data$UnitPrice)
stat.desc(data$UnitPrice)
boxplot(data$UnitPrice, main = "Boxplot - Unit Price")
hist(data$UnitPrice)
hist(data$UnitPrice[which(abs(data$UnitPrice)<100)])

###########################################
# Variable 7 - CustomerID (Categorical)
##########################################
length(unique(data$CustomerID))#4373
#length(unique(complete_data$CustomerID))#4372

table(data$CustomerID)
# histogram for CustomerID
hist(table(data$CustomerID))


###########################################
# Variable 8 - Country (Categorical)
##########################################
length(unique(data$Country))#38
#length(unique(complete_data$Country))#37
table(data$Country)
# histogram for Country
hist(table(data$Country))

###########################################
# Variable 9 - TotalPrice (Continuous)
##########################################
# Create a new variable "Total Price"
data <- data %>% 
  mutate(TotalPrice = Quantity*UnitPrice)

complete_data <- complete_data %>% 
  mutate(TotalPrice = Quantity*UnitPrice)

boxplot(data$TotalPrice, main = "Boxplot - Unit Price")
hist(data$TotalPrice)
hist(data$TotalPrice[which(abs(data$TotalPrice)<100)])


#####################################
# BI-VARIATE ANALYSIS
####################################


data %>% group_by(CustomerID) %>% summarise(visits = n_distinct(InvoiceNo)) %>% 
  ggplot(aes(log(visits))) + geom_histogram() + labs(x="No. of Trips", y= "Customer Count")

# options(repr.plot.width=8, repr.plot.height=3)
# data %>% group_by(InvoiceDate) %>% summarise(revenue = sum(TotalPrice)) %>% 
#   ggplot(aes(x=InvoiceDate, y=revenue/1000000)) + geom_line() +
#   labs(x="Period", y="Revenue in million")




# Continuous-Continuous Variable
plot(data$TotalPrice,data$Quantity)
cor(data$TotalPrice,data$Quantity)

plot(data$UnitPrice,data$Quantity)
cor(data$UnitPrice,data$Quantity)

monthlySummary <- data %>%
  group_by(Date, Month) %>%
  summarise(revenue = sum(TotalPrice), transactions = n_distinct(InvoiceNo)) %>%
  mutate(AverageBasketValue = (round((revenue / transactions),2))) %>%
  ungroup()

monthlySummary$`data$Month` <- as.Date(monthlySummary$`data$Month`, '%m')
head(monthlySummary, n = 10)

#Revenue by Month
ggplot(monthlySummary, aes(x = Month, y = revenue)) + 
  geom_boxplot() + labs(x = 'Month', y = 'Revenue', title = 'Revenue by Month')
#Number of Transactions by Month
ggplot(monthlySummary, aes(x = Month, y = transactions)) + geom_boxplot() + 
  labs(x = 'Month', y = 'Number of Daily Transactions', title = 'Number of Transactions by Month')

#Average Order Value by Month
ggplot(monthlySummary, aes(x = Month, y = AverageBasketValue)) + geom_boxplot() + 
   labs(x = 'Month', y = 'Average Order Value', title = 'Average Order Value by Month')

ggplot(monthlySummary, aes(transactions, fill = Month)) + geom_density(alpha = 0.2)

# Continuous-Categorical
#Sales by Invoice
data_Inv <- data %>% 
  group_by(InvoiceNo) %>% 
  summarise(SumTotalPrice=sum(TotalPrice),TotalQty=sum(Quantity))
SI<- data_Inv

plot(SI$SumTotalPrice,SI$TotalQty)
summary(SI)
getmode(SI$TotalQty)
getmode(SI$SumTotalPrice)

#Sales by Customer
data_cust <- data %>% 
  group_by(CustomerID) %>% 
  summarise(SumTotalPrice=sum(TotalPrice),TotalQty=sum(Quantity))
SC<- data_cust

plot(SC$SumTotalPrice,SC$TotalQty)
summary(SC)
getmode(SC$TotalQty)
getmode(SC$SumTotalPrice)

#Sales by Product
data_prod <- data %>% 
  group_by(StockCode) %>% 
  summarise(SumTotalPrice=sum(TotalPrice),TotalQty=sum(Quantity))
SP<- data_prod

plot(SP$SumTotalPrice,SP$TotalQty)
summary(SP)
getmode(SP$TotalQty)
getmode(SP$SumTotalPrice)

#Sales by Country
data_country <- data %>% 
  group_by(Country) %>% 
  summarise(SumTotalPrice=sum(TotalPrice),TotalQty=sum(Quantity),CustCount=length(unique(CustomerID)))
SC<- data_country

plot(SC$SumTotalPrice,SC$TotalQty)
plot(SC$Country,SC$CustCount,type="h")
summary(SC)
getmode(SC$TotalQty)
getmode(SC$SumTotalPrice)

#ggplot(weekdaySummary, aes(transactions, fill = dayOfWeek)) + geom_density(alpha = 0.2)






# Categorical-Categorical


#####################################
# Multi-VARIATE ANALYSIS
####################################

options(repr.plot.width=8, repr.plot.height=6)
data %>% filter(Country != "United Kingdom") %>% group_by(Country) %>% 
  summarise( revenue = sum(TotalPrice))%>% arrange(desc(revenue, Country))

countries<- c("Netherlands", "EIRE", "Germany", "France", "Austraia", "Spain", "Switzerland")
countries

options(repr.plot.width=6, repr.plot.height=6)
data %>% filter(Country %in% countries) %>% group_by(Date, Country) %>% summarise(Revenue = sum(TotalPrice)) %>%
  ggplot(aes(x=Date, y=Revenue, col=Country)) + geom_smooth(method ="loess", se=F)


# options(repr.plot.width=8, repr.plot.height=6)
# data %>% filter(Country == "United Kingdom") %>% group_by(Country) %>% 
#   summarise( revenue = sum(TotalPrice))%>% arrange(desc(revenue, Country))
# 
# country <- c("United Kingdom")
# country
# options(repr.plot.width=6, repr.plot.height=6)
# data %>% filter(Country %in% country) %>% group_by(Date, Country) %>% summarise(Revenue = sum(TotalPrice)) %>%
#   ggplot(aes(x=Date, y=Revenue, col=Country)) + geom_smooth(method ="loess", se=F)

#############################################
# MISSING VALUES IDENTIFICATION AND TREATMENT
#############################################

#Missing Value
plot_missing(data)
#plot_missing(data)
#Missing Description
count(data[data$Description=="",])




################################################
# IMPOSSIBLE VALUES IDENTIFICATION AND TREATMENT
################################################

#Negative Price
count(data[data$UnitPrice<=0,])

#Negative Quantity but not Cancelled items
count(data[data$Quantity<=0 & data$Cancelled=="N",])
count(data[data$Quantity<=0 & data$Cancelled=="N",])

#Invalid Descriptions
length(unique(data$Description[str_length(data$Description)<=8]))
#warnings()
length(unique(data$Description[str_length(data$Description)<=8]))

###Non-Numeric Stock Codes
sort(unique(data$StockCode),decreasing=TRUE)[1:33]
sort(unique(data$StockCode),decreasing=TRUE)[1:8]

################################################
# OUTLIERS IDENTIFICATION AND TREATMENT
################################################

#Box Plot for Outliers
plot_boxplot(data, by="Country",geom_boxplot_args = list("outlier.color" = "red"))





################################################
# DATA CLEANUP
################################################


# #Remove missing customer repords
# trans<- trans[-which(is.na(trans$CustomerID)),]
# #summary(trans$CustomerID)
# 
# #Remove records which are outliers
# trans[which(abs(trans$Quantity)>50000),]
# trans<- trans[-which(abs(trans$Quantity)>50000),]
# 
# head(trans[which(trans$Quantity<0),])
# head(trans[which(trans$UnitPrice<0.1),])
# 
# trans[which(abs(trans$UnitPrice)>5000),]
# trans<- trans[-which(abs(trans$UnitPrice)>5000),]




#trans[which(abs(trans$Quantity)>20000),]
#trans[which(abs(trans$UnitPrice)>5000),]
#sum(trans$Quantity[which(abs(trans$UnitPrice)==0)])
#sum(trans$Quantity)