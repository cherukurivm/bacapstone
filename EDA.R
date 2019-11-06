# Referenced frim https://www.kaggle.com/chinarxo/e-commerce-eda-clustering
suppressMessages(library(tidyverse)) 
suppressMessages(library(purrr)) 
suppressMessages(library(lubridate)) 
suppressMessages(library(funModeling))
suppressMessages(library(ggplot2))
suppressMessages(library(DataExplorer))
suppressMessages(library(data.table))
suppressMessages(library(Hmisc))
data <- read.csv("data.csv")
str(data)
describe(data)
summary(data$Quantity)
summary(data$UnitPrice)
#hist(data$CustomerID)
df_status(data)
plot_missing(data)
map_df(data, function(x) sum(is.na(x)))
### Exploring the Data 
par(mfrow=c(2,2))
boxplot(data$Quantity, main ="Boxplot- Qty")
boxplot(data$UnitPrice, main = "Boxplot - Unit Price")
data<- data %>% mutate(Sale_amount = Quantity * UnitPrice)
data_pos<-data %>% filter(Sale_amount>0)
data_neg <- data %>% filter (Sale_amount<=0)
data_pos <- data_pos %>% na.omit()
data_pos$InvoiceDate<- as.character(data_pos$InvoiceDate)
data_pos$Date <- sapply(data_pos$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
data_pos$Time <- sapply(data_pos$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})
data_pos$Month <- sapply(data_pos$Date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][1]})
data_pos$Year <- sapply(data_pos$Date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][3]})
data_pos <- data_pos[,-5]
str(data_pos)
data_pos$CustomerID <- as.factor(data_pos$CustomerID)
data_pos$Date <- as.Date(data_pos$Date, '%m/%d/%Y')
data_pos$Year <- as.factor(data_pos$Year)
data_pos$Month <- as.factor(data_pos$Month)
data_pos$Month <- month.abb[data_pos$Month]
data_pos$Month <- as.factor(data_pos$Month)
data_pos %>% group_by(CustomerID) %>% summarise(visits = n_distinct(InvoiceNo)) %>% 
  ggplot(aes(log(visits))) + geom_histogram() + labs(x="Trips", y= "No of customers")
options(repr.plot.width=6, repr.plot.height=3)
data_pos %>% group_by(StockCode, Description) %>% summarise(count= n()) %>% arrange(desc(count)) %>% head() %>%
  ggplot(aes(x=Description, y=count, fill = count)) + geom_bar(stat= "identity") + coord_flip() + 
  labs(y="Number of items purchased", x="Product")
options(repr.plot.width=6, repr.plot.height=3)
data_pos %>% group_by(StockCode, Description) %>% summarise(count= n()) %>% filter(count<10)
options(repr.plot.width=4, repr.plot.height=4)
data_pos %>% group_by(Year) %>% summarise(total_sales= sum(Sale_amount)) %>% 
  ggplot(aes(x= Year, y= total_sales/1000000, fill = Year)) + 
  geom_bar(stat="identity") + labs(x="Year", y="Total Sales in million")
options(repr.plot.width=8, repr.plot.height=3)
data_pos %>% group_by(Date) %>% summarise(revenue = sum(Sale_amount)) %>% 
  ggplot(aes(x=Date, y=revenue/1000000)) + geom_line() +
  labs(x="Period", y="Revenue in million")
data_pos %>% arrange(desc(Sale_amount)) %>% head()
data_neg %>% arrange(Sale_amount) %>% head()
drop_records<-which(data_pos$Sale_amount> 38000)
drop_records
data_pos<- data_pos[-drop_records,]
options(repr.plot.width=8, repr.plot.height=3)
data_pos %>% group_by(Date) %>% summarise(revenue = sum(Sale_amount)) %>% 
  ggplot(aes(x=Date, y=revenue/1000000)) + geom_line() +
  labs(x="Period", y="Revenue in million")
options(repr.plot.width=6, repr.plot.height=8)
data_pos %>% group_by(Country) %>% summarise(revenue = sum(Sale_amount)) %>% 
  ggplot(aes(y=revenue/1000000, x=Country)) + geom_bar(stat="identity") +
  labs(x="Country", y="Sales in million") + coord_flip()

data_pos %>% filter(Year==2011, Country != "United Kingdom") %>% group_by(Year, Country) %>% 
  summarise(revenue = sum(Sale_amount)) %>% arrange(desc(revenue))
options(repr.plot.width=8, repr.plot.height=6)
data_pos %>% filter(Country != "United Kingdom") %>% group_by(Country) %>% 
  summarise( revenue = sum(Sale_amount))%>% arrange(desc(revenue, Country))

countries<- c("Netherlands", "EIRE", "Germany", "France", "Austraia", "Spain", "Switzerland")
countries
options(repr.plot.width=6, repr.plot.height=6)
data_pos %>% filter(Country %in% countries) %>% group_by(Date, Country) %>% summarise(Revenue = sum(Sale_amount)) %>%
  ggplot(aes(x=Date, y=Revenue, col=Country)) + geom_smooth(method ="loess", se=F)
options(repr.plot.width=5, repr.plot.height=5)
data_pos %>% group_by(Month, Year) %>% filter(Country != "United Kingdom") %>% summarise( Revenue = sum(Sale_amount)) %>%
  ggplot(aes(x=Month, y= Revenue, fill = Year)) + geom_bar(stat="identity", position ="dodge") + coord_flip()



