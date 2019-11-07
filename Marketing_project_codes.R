
# Welcome to the Business Analytics Capstone project code. The below code book will help you to learn few of the crucial 
# codes in the field of business analytics which will help you to conduct the analysis for the project of the chosen type.
# I prefer to use RStudio as the software on which I will run the R based scripts.
# Also, I will not directly demonstrate the solutions and a direct pathway for the project,
# but instead, this code book will contain the step by step description about what is possible from each type of code.

# Every language has additional scripts written by developers throughout the world which contains 
# pre-defined codes to help students to conduct the required analysis they want.
# But before using them, we need to install those collection of scripts which are called libraries.
# The way we have libraries in our schools or universities which contains books,
# these libraries books in the form of "scripts"

# The first step of the project is to download R(use the below link):
# https://www.r-project.org

# Once done, I prefer to work on RStudio which you can download from the below link:
# https://rstudio.com/products/rstudio/download/


# Let's install the important libraries that we will use for the project.

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



# Once the package is installed, you have to load them to in your script to use them.
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



# In every programming language, files are stored at a default path.
# To change the view the default path from where the RStudio or R terminal will read your data is found using the below command:
# Type it in RStudio:

getwd()
# You should get the output like this in case of MacOS
# "/Users/mohitgupta"

# In order to change the location of your directory:
setwd("/Users/mohitgupta/BA_Cap")

# It is the location where you want to store your files and data which you will read for the project.
# Please make sure that whatever data you are loading is stored in the location you set.

# It's time to load the data into RStudio
# Let's save the data in a variable that we can name according to our choice:

data <- read.csv('ecommerce.csv')

# Taking a glimpse at the data:
glimpse(data)


# Let's check the shape of the data

# This gives the number of rows x columns
dim(data)

# The below function calculates the number of missing values:
# The function which is specifically used to check if the data set contains the missing values: is.na()
paste('Number of Missing Values:', sum(is.na(data)))

# Using the apply function, you will be able to observe that how many values are missing in each column
apply(data, 2, function(x){sum(is.na(x))})
# In the current case, no values are missing from all the columns except CustomerID

# To further analyze the unique number of attributes about the missing values, we can run the below function:

# In a data, to make a subset of the data which has all the rows with missing values in any row, we use the below code
# In bracket, we are supposed to enter two values:
# name_of_dataset[row number or row range, column number or column range]
# This is called indexing
# You can visit the below website to learn more in depth about it.
# https://rspatial.org/intr/4-indexing.html 


missing_data <- data[is.na(data$CustomerID),] # THis data set contains all the rows where customer ID is missing

complete_data <- data[!is.na(data$CustomerID),] # This dataset contains all the rows where customer ID is not missing


# You may try to see whether these customer IDs can be found by looking for the "invoice_numbers" from missing_data file in the 
# complete_data file. The logic is one invoice may not belong to two different customers 




# We use the ' $ ' symbol to get a specific column from a dataframe
# for example if you want to get a mean of "col1" from your data frame df
# you can use this command - mean(df$col1)

mean(complete_data$UnitPrice)
#  3.460471


# I fyou want to get a frequency table with the number of obsrevations for each category in a categorical variable such as 
# country, use the following command
table(complete_data$Country)

# creating a histogram of from the data in the above table 
hist(table(complete_data$Country))

# Let's do an analysis on what kind of unique data we have in the table for every specific row:

# Number of unique Invoice Numbers
length(unique(data$InvoiceNo))

# we can use the below command to print it 
paste('Number of Unique Inoivce:' ,length(unique(data$InvoiceNo)))

# Number of unique Stock codes
paste('Number of Unique Stock Code (items):', length(unique(data$StockCode)))

# Number of unique item Descriptions
paste('Number of Categories of the item:', length(unique(data$Description)))

# Number of unique Customer IDs
paste('Number of Uniqe Customer id:', length(unique(data$CustomerID)))

# Number of unique Countries
paste('Number of Unique Country:', length(unique(data$Country)))



# Let's find the distribution of customers from each country:

customer_country <- data %>% select(CustomerID, Country) %>% group_by(Country) %>% summarise(num_customers = n())
# In the code above we first select the dataframe, then select only two columns "customerID" and "country", 
# then aggregate (using summarise function) the data so that each row becomes a country and the other column shows the number of customers from 
# each country

# we will explain more on the group_by and summarise 

# the customer_country created above is a dataframe with two variables, country and the count of customers from each of these 
# countries

# In the above code, %>% is called 'piping'.
# Piping was not originally included in R but is very extensively used in dplyr library

# What it means is:
# data %>% head()
# Pass every element of the data into the head function
# This is also equivalent to head(data)

# This is very easy to use in cases when one function is applied inside another.


### ggplot for graphing 

# ggplot is one of the most extensively used data visualization library

# Let's create one of the charts to get a clear picture of what country has the highest number of invoices.

ggplot(data=customer_country[customer_country$Country != 'United Kingdom',], aes(x=Country, y=num_customers)) +
  ggtitle('Number of customers Per Country') + xlab("Country") + ylab("Number of customers") + 
  geom_bar(stat="identity", width=0.5) + 
  theme(axis.text.x=element_text(angle=90, hjust=1), 
        plot.title = element_text(hjust = 0.5))




# group_by function is used to make groups of table inside the table to seperate similar data from either one row
# or either from multiple rows.

# The code below calculates the number of transactions by each customer
transactions_per_customer<- summarize(complete_data %>% select(CustomerID,InvoiceNo) %>% group_by(CustomerID), transactions_count = unique(n()))

# sort customers in descending order by the number of tranactions
transactions_per_customer <- arrange(transactions_per_customer, desc(transactions_count))


# Shows top 5 customers in terms of numbers of transactions done
# Customer ID: Number of bills on his/her name
head(transactions_per_customer)


# Function to create histogram for the # of transactions 
hist(transactions_per_customer$transactions_count, breaks=10000, xlim=c(0,500),
     main='Histogram of transactions per customer', xlab='Number of transactions', ylab='Number of customers with X number of bills')


# let's check in what format is invoice date saved
class(data$InvoiceDate)
# "factor"
# we will need to change the date from factor type to date class type otherwise we can't do much manipulation on the date. 

# Let's convert the Invoicedate into a date format
data$InvoiceDate <- strptime(x = as.character(data$InvoiceDate), format = "%m/%d/%Y %H:%M")
# New date format: 
head(data$InvoiceDate)


# Let's extract the date and hour from the new dataset
date_customer <- data %>% select(InvoiceDate, CustomerID)
date_customer$date <- date(date_customer$InvoiceDate)
head(date_customer$date)

date_customer$hour <- hour(date_customer$InvoiceDate)
head(date_customer$hour)



# Let's try to find the time during which most products were offered.
hour_range <- summarize(date_customer %>% select(CustomerID, hour) %>% group_by(hour), count = n())



# The below range function gives us the range of lowest to the highest value of the dates
range(date_customer$date)

# Check for missing dates
unique_date <- unique(date_customer$date)
head(unique_date)

# The which() function is specifically used to find the index of the parameters which are passed to it. 

missing_date_index <- which((c(unique_date, NA) - c(NA, unique_date)) > 1)
head(missing_date_index)

# Once we got the index about the missing dates, we will pass it 
missing_date <- unique_date[missing_date_index-1] +1
missing_date

# On the basis of above obtained values, the names of weekdays and weekends are found which happens to occur on missing dates.
weekdays(missing_date) 


# Below is the minimum and maximum hours in between of which customers place the order.
range(date_customer$hour)

# Below is the method used to see that how many orders were made during a certain day.
# This will help us to analyse the most busy hours in a day and the clients that the business handles on a particular day
customer_per_day <- date_customer %>% group_by(date)%>% summarise(count = n_distinct(CustomerID))

# Let's plot this
p1 <- ggplot(data=customer_per_day, aes(x=date, y=count)) +
  geom_line() +
  ggtitle("Number of Customers Every Day") + 
  theme(panel.background = element_rect(fill='white', colour = 'grey'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

p1  # write this to show the plot
# Below is the method used to see that at what time of the day, more products are ordered.

customer_per_hour <- date_customer %>% group_by(hour)%>% summarise(count = n_distinct(CustomerID))

p2 <- ggplot(data=customer_per_hour, aes(x=hour, y=count)) +
  geom_line() +
  ggtitle("Number of Customers Every Hour") + 
  theme(panel.background = element_rect(fill='white', colour = 'grey'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

p2

grid.arrange(p1, p2, nrow = 2)


# Now let's also list down different kinds of analysis that we can perform for other charts.
# 1. Which country bought the goods with highest value.

# Let's also try one more variation here.
# Making one more column to depict the total value of the good purchased by country

purchase_value_country <- data %>% select(Country, UnitPrice) %>% group_by(Country) %>%  summarise(Total_sum = sum(UnitPrice)) 
purchase_value_country <- as.data.frame(purchase_value_country)

# Arranging the data in the ascending order of the desired column
# This way you can see that which country ordered the least monetary value worth of goods.

# You can use ARRANGE function from the dplyr package for this in the following way
purchase_value_country <- arrange(purchase_value_country,desc(Total_sum)) 

head(purchase_value_country)

# # if you remove the desc, it will sort them in increasing order
purchase_value_country <- arrange(purchase_value_country,Total_sum)

head(purchase_value_country)



# Making another level of variation by introducing grouping by multiple columns
purchase_value_country_year <- data %>% select(Country, InvoiceDate, UnitPrice)
purchase_value_country_year$Year <- year(purchase_value_country_year$InvoiceDate)
purchase_value_country_year<- purchase_value_country_year  %>% group_by(Country, Year) %>% summarise(Total_sum = sum(UnitPrice)) 

head(purchase_value_country_year)

p<-ggplot(purchase_value_country_year[purchase_value_country_year$Country != 'United Kingdom',], 
          aes(x=Country, y=Total_sum, fill= Year)) + geom_bar(stat="identity") 


# As the bar doesn't looks good, I'll now flip it.
p 

p + coord_flip()





# 2. Which month observes the highest sales in terms of monetary value


purchase_value_country_month <- data %>% select(Country, InvoiceDate, UnitPrice)
purchase_value_country_month$Month <- month(purchase_value_country_month$InvoiceDate)
purchase_value_country_month <- purchase_value_country_month  %>% group_by(Country, Month)%>% summarise(Total_sum = sum(UnitPrice)) 
head(purchase_value_country_month)

p<-ggplot(purchase_value_country_month[purchase_value_country_month$Country != 'United Kingdom',], aes(x=Month, y=Total_sum, group=Country)) +
  geom_line(aes(color=Country))+ geom_point(aes(color=Country))
p


# 3. Which are the most sold top 10 products

top_10_products <- data %>% select(Description, UnitPrice)
top_10_products <- summarize(top_10_products %>% group_by(Description), Total_sum = sum(UnitPrice))

top_10_products <- as.data.frame(top_10_products)
top_10_products <- head(top_10_products[order(top_10_products$Total_sum,decreasing = TRUE), ], 10)
top_10_products


#------------------------------------------------------


# Segmentation of Customer using RFM analysis 

id1 = which (data$Quantity<0) # if you choose to remove the negtaive quantity rows
id2 = which (data$UnitPrice<0) # if you choose to remove the negtaive price rows
id3 = which (is.na(data$CustomerID)) # if you want to drop the rows where no customerID is available 
id = unique(c(id1,id2,id3)) 
newdata <- data[-id,] # rmeoving all of the baove cases 
newdata$InvoiceNo=as.factor(newdata$InvoiceNo)
newdata$StockCode=as.factor(newdata$StockCode)
newdata$InvoiceDate=as.Date(newdata$InvoiceDate, '%m/%d/%Y %H:%M')

newdata$CustomerID=as.factor(newdata$CustomerID)
newdata$Country=as.factor(newdata$Country)
totalPrice = newdata$Quantity * newdata$UnitPrice
newdata= cbind(newdata, totalPrice)
# calculating RFM
RFM <- newdata %>%
  group_by(CustomerID) %>%
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo),
            monetary= sum(totalPrice)/ frequency)


# RFM Analysis 
hist(RFM$frequency, breaks = 20)
hist(RFM$monetary, breaks = 20)
hist(RFM$recency, breaks =20)

RFM2 <- RFM[,2:4] # select the RFM columns for clustering. 

# you will need to standardize your variables R,F and M so that their variances are comparable.
# This is a standard practice in case of any algorithm that uses distance. For exmaple, K Nearest Neighbour
RFM$recency <- scale(RFM2$recency, center = TRUE, scale = TRUE) # center specifies that mean =o and scale specifies that var =1 
RFM$frequency <- scale(RFM2$frequency, center = TRUE, scale = TRUE)
RFM$monetary <- scale(RFM2$monetary, center = TRUE, scale = TRUE)
## K-means
km2=kmeans(RFM2, centers=2, nstart=10) # the argument centers specifies the number of clusters 

# Save the cluster number in the dataset as column 'cluster"

RFM2$cluster <- as.factor(km2$cluster)

km3=kmeans(RFM2, centers=3, nstart=10) # for k =3
RFM2$cluster <- as.factor(km2$cluster)



par(mfrow=c(1,2));
plot(Xmds[,1], Xmds[,2], type="n", xlab="", ylab="")
points(Xmds[,1], Xmds[,2], pch=km2$cluster, col=km2$cluster);
title("MDS plot for K=2")




# Association Rule Mining and Collabortaive Filtering 


