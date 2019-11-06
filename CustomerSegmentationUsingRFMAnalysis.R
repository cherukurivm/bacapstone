# https://www.kaggle.com/hendraherviawan/customer-segmentation-using-rfm-analysis-r
library(data.table)
library(dplyr)
library(ggplot2)
#library(stringr)
#library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)

#Load & Examine Dataset
df_data <- fread('data.csv')
glimpse(df_data)

# Delete all negative Quantity and Price. We also need to delete NA customer ID
df_data <- df_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))

df_data <- df_data %>%
  drop_na()


# Recode and convert character variables to factors.

df_data <- df_data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))
#as.Date(data$InvoiceDate,"%d-%m-%y %H:%M")

df_data <- df_data %>% 
  mutate(total_dolar = Quantity*UnitPrice)

glimpse(df_data)

# Calculate RFM
# To implement the RFM analysis, we need to further process the data set in by the following steps:
#   
# Find the most recent date for each ID and calculate the days to the now or some other date, to get the Recency data
# Calculate the quantity of translations of a customer, to get the Frequency data
# Sum the amount of money a customer spent and divide it by Frequency, to get the amount per transaction on average, that is the Monetary data.


df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), monitery= sum(total_dolar)/n_distinct(InvoiceNo)) 

summary(df_RFM)

kable(head(df_RFM))

# Recency – How recently did the customer purchase?
  
hist(df_RFM$recency)

# Frequency – How often do they purchase?
  
hist(df_RFM$frequency, breaks = 50)

#Monetary Value – How much do they spend?
  
hist(df_RFM$monitery, breaks = 50)


#Clustering
df_RFM2 <- df_RFM
row.names(df_RFM2) <- df_RFM2$CustomerID

df_RFM2$CustomerID <- NULL

df_RFM2 <- scale(df_RFM2)
summary(df_RFM2)

d <- dist(df_RFM2)
c <- hclust(d, method = 'ward.D2')

plot(c)
# Cut the tree into groups of data

members <- cutree(c,k = 8)

members[1:5]

table(members)


aggregate(df_RFM[,2:4], by=list(members), mean)
