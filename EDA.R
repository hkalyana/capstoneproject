 library(dplyr)
 library(car)
 library(plyr)
 library(zoo)
 library(MASS)
 library(DAAG)
 library(lubridate)

#Import the eleckart dataset
eleckart_consumer_data <- read.csv("ConsumerElectronics.csv", stringsAsFactors = TRUE, header = TRUE)

str(eleckart_consumer_data)
summary(eleckart_consumer_data)


nrow(eleckart_consumer_data) #1648824
ncol(eleckart_consumer_data) # 20
colnames(eleckart_consumer_data)


# Convert Date into POSIXlt
# seggregate the data (order date) into -
# i) Year
# ii)month
# iii) date
# iv ) hour
# v) minute
# Filter data based on the given date range -

#Uncomment below line when executing this code first time.
#install.packages("lubridate") # Could be optimize by using %W





#Seggregating the date into hours, minute and date of the month
eleckart_consumer_data$order_date <- as.POSIXct(eleckart_consumer_data$order_date, format = "%Y-%m-%d %H:%M:%S")
eleckart_consumer_data$order_day <- format(eleckart_consumer_data$order_date, "%d")
eleckart_consumer_data$order_hour <-format(eleckart_consumer_data$order_date, "%H")
eleckart_consumer_data$order_minute <-format(eleckart_consumer_data$order_date, "%M")
eleckart_consumer_data$order_week<-lubridate::week(eleckart_consumer_data$order_date)
#This is giving wrong mapping of weeks for saturdays. Need to check.
nrow(eleckart_consumer_data) 
max(eleckart_consumer_data$order_week)

#You have to use the data from July 2015 to June 2016.
eleckart_consumer_data <- subset(eleckart_consumer_data, order_date > "2015-6-30" & order_date < "2016-7-1") #1648215


nrow(eleckart_consumer_data) #1648215
str(eleckart_consumer_data)

#Since the model building will happen on a weekly basis extracing week of the year from the order date

eleckart_consumer_data$order_week<- ifelse(eleckart_consumer_data$order_week<=26 & eleckart_consumer_data$Year==2016,eleckart_consumer_data$order_week+53,eleckart_consumer_data$order_week)

summary(eleckart_consumer_data$order_week)
nrow(eleckart_consumer_data)
#1648215 Revisit this

colSums(is.na(eleckart_consumer_data))
#We have 4904 rows of data empty for cust_id,pincode and gmv and it is very small compared to the data set
# removing rows with NA values
eleckart_consumer_data <- na.omit(eleckart_consumer_data)
nrow(eleckart_consumer_data)#1643311


#TODO
#eleckart_consumer_data$date<-as.Date(eleckart_consumer_data$Order_date)

#Analysing product_mrp column
summary(eleckart_consumer_data$product_mrp)

nrow(subset(eleckart_consumer_data,product_mrp ==0))#5290 Which is a small number. Hence filtering them out.
eleckart_consumer_data <- subset(eleckart_consumer_data,product_mrp>0)

#Analyzing the dependent variable gmv column
nrow(subset(eleckart_consumer_data,gmv ==0))#985 so filter them out as well.
eleckart_consumer_data <- subset(eleckart_consumer_data,gmv>0)
nrow(eleckart_consumer_data)

#abc<- sqldf:: sqldf("select * from eleckart_consumer_data group by fsn_id")
# Fact: you can sell items at a price less than or equal to product_mrp and not go beyond that.
# check if gmv is > units in product_mrp
nrow(subset(eleckart_consumer_data, gmv > product_mrp*units)) #33632
View(subset(eleckart_consumer_data, gmv > product_mrp*units))
#Remove all such records from the dataset.
eleckart_consumer_data <- subset(eleckart_consumer_data, gmv <= product_mrp*units)
nrow(eleckart_consumer_data)#1603404

#Derived Metrics /KPI

#Payment mode Prepaid =1 and COD =0 TODO: Flip the flag
eleckart_consumer_data$payment_mode<-ifelse(eleckart_consumer_data$s1_fact.order_payment_type=="COD",0,1)
summary(eleckart_consumer_data)
#Remove s1_fact.order_payment_type
#eleckart_consumer_data <- eleckart_consumer_data[,-11]

library(sqldf)
# 
# total_orders_by_week<- sqldf('select order_week, count(1) as count from eleckart_consumer_data group by order_week')
# total_prepaid_orders_by_week<- sqldf('select order_week, count(1) as count from eleckart_consumer_data group by order_week having payment_mode =1')
# 
# prepaid_order_precentage<- 

summary(eleckart_consumer_data$order_week)
#Calculate the list price
View(eleckart_consumer_data)

eleckart_consumer_data$list_price <- eleckart_consumer_data$gmv/eleckart_consumer_data$units

#Calculate the discount/markdown percentage TODO: Change the name to markdown
eleckart_consumer_data$markdown <- (eleckart_consumer_data$product_mrp-eleckart_consumer_data$list_price)/eleckart_consumer_data$product_mrp


#Removing columns that are not required.
colnames(eleckart_consumer_data)

#1. s1_fact.order_payment_type
#2. product_analytic_super_category
#3. product_analytic_category
#4. product_analytic_vertical
eleckart_consumer_data <- eleckart_consumer_data[,-c(11,15,16,18)]

colnames(eleckart_consumer_data)

#Load the other metadata given.
investment_data <- read.csv("investment.csv", stringsAsFactors = TRUE, header = TRUE)
nps_data<- read.csv("nps.csv", stringsAsFactors = TRUE, header = TRUE)
sale_data<- read.csv("sale.csv", stringsAsFactors = TRUE, header = TRUE)


#Merge NPS data
eleckart_consumer_data<-merge(eleckart_consumer_data,nps_data,by=c("Month","Year"),all.x=TRUE)

#Merge sale_data
sale_data$order_week <- week(ISOdate(sale_data$Year, sale_data$Month, sale_data$Day))

sale_data$order_week<- ifelse(sale_data$order_week<=26 & sale_data$Year==2016,sale_data$order_week+53,sale_data$order_week)


promotion_days_in_week <- sqldf("select count(1) count_promotion_days_in_week,order_week from sale_data group by order_week")

sale_data <- merge(sale_data,promotion_days_in_week, by=c("order_week"), all.x = TRUE)
sale_data<- sale_data[,-c(2,3,4,5)]
sale_data<- unique(sale_data)

# 
total_orders_by_week<- sqldf('select order_week, count(1) as orders_by_week from eleckart_consumer_data group by order_week')
total_prepaid_orders_by_week<- sqldf('select order_week, count(1) as prepaid_orders_by_week from eleckart_consumer_data where payment_mode =1 group 
                                     by order_week')
# 
order_aggregation<- sqldf("select a.order_week,a.orders_by_week,b.prepaid_orders_by_week from total_orders_by_week a join 
                          total_prepaid_orders_by_week b on a.order_week=b.order_week")

order_aggregation$prepaid_percentage <- order_aggregation$prepaid_orders_by_week/order_aggregation$orders_by_week
order_aggregation<-order_aggregation[,-c(2,3)]

eleckart_consumer_data<- merge(eleckart_consumer_data,order_aggregation,by="order_week",all.x = TRUE)


gamingAccessoryDF <-eleckart_consumer_data[eleckart_consumer_data$product_analytic_sub_category=="GamingAccessory" ,]

cameraAccessoryDF<-eleckart_consumer_data[eleckart_consumer_data$product_analytic_sub_category=="CameraAccessory",]

homeAudioDF<-eleckart_consumer_data[eleckart_consumer_data$product_analytic_sub_category=="HomeAudio",]

#Writing to 3 different CSV.
write.csv(cameraAccessoryDF, file = "cameraAccessory.csv",row.names=FALSE)
write.csv(homeAudioDF, file = "homeAudio.csv",row.names=FALSE)
write.csv(gamingAccessoryDF, file = "gamAccessory.csv",row.names=FALSE)

########################################################################################################################################################################################################################################################################################################################################################################

# EDA analysis strategy -  

# 1) check total null values 
# 2) check which all columns have null values and how can those null values can be treated 
# 3) check data quality for every column like pin code value is non negative etc 
# 4) Univariate and BiVariate analysis based on the - 
# i)   Year 
# ii)  month
# iii) date 
# iv ) hour
# v)   minute
# 5) Perfrom bivariate analysis for 
# i)   Time vs units
# ii)  shipment type
# iii) pin code vs quantity 

#Univariate,  BiVariate & MultiVariate Analysis 
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
  
  library(dplyr)
## Number of orders year, month, day, week and hour basis
order_year_group <-  group_by(eleckart_consumer_data, Year)
order_month_group <- group_by(eleckart_consumer_data,Month) 
order_week_group <-  group_by(eleckart_consumer_data, order_week) 
order_day_group <-   group_by(eleckart_consumer_data,  order_day)
order_hour_group <-  group_by(eleckart_consumer_data, order_hour)
order_pincode_group <- group_by(eleckart_consumer_data, pincode) 
order_fsn_id_group <- group_by(eleckart_consumer_data, fsn_id) 
#order_paymentType_group <- group_by(eleckart_consumer_data, s1_fact.order_payment_type) 



order_year_group_summary <- summarise(order_year_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_year_group_summary, desc(count))

order_month_group_summary <- summarise(order_month_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_month_group_summary, desc(count))


order_week_group_summary <- summarise(order_week_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_week_group_summary, desc(count))


order_day_group_summary <- summarise(order_day_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_day_group_summary, desc(count))

order_hour_group_summary <- summarise(order_hour_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_hour_group_summary, desc(count))


order_pincode_group_summary <- summarise(order_pincode_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_pincode_group_summary, desc(count))

order_fsn_id_group_summary <- summarise(order_fsn_id_group, count=n(), mean = mean(units), sum= sum(units), meangvm = mean(gmv), sumgvm = sum(gmv)) 
arrange(order_fsn_id_group_summary, desc(count))


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
a<-ggplot(order_year_group_summary, aes(x=Year, y=meangvm, fill=factor(Year))) + geom_bar(position="dodge", stat="identity")
b<-ggplot(order_year_group_summary, aes(x=Year, y=mean, fill=factor(Year))) + geom_bar(position="dodge", stat="identity")
c<-ggplot(order_year_group_summary, aes(x=Year, y=count, fill=factor(Year))) + geom_bar(position="dodge", stat="identity")
d<-ggplot(order_year_group_summary, aes(x=Year, y=sum, fill=factor(Year))) + geom_bar(position="dodge", stat="identity")
e<-ggplot(order_year_group_summary, aes(x=Year, y=sumgvm, fill=factor(Year))) + geom_bar(position="dodge", stat="identity")

#install.packages("cowplot")
library("cowplot")
plot_grid(a, b, c,d,e , 
          labels = c("A", "B", "C", "D","E"),
          ncol = 2, nrow = 3)


#a1<-ggplot(order_month_group_summary, aes(x=Month, y=meangvm, fill=factor(Month))) + geom_bar(position="dodge", stat="identity")
b1<-ggplot(order_month_group_summary, aes(x=Month, y=mean, fill=factor(Month))) + geom_bar(position="dodge", stat="identity")
c1<-ggplot(order_month_group_summary, aes(x=Month, y=count, fill=factor(Month))) + geom_bar(position="dodge", stat="identity")
d1<-ggplot(order_month_group_summary, aes(x=Month, y=sum, fill=factor(Month))) + geom_bar(position="dodge", stat="identity")
e1<-ggplot(order_month_group_summary, aes(x=Month, y=sumgvm, fill=factor(Month))) + geom_bar(position="dodge", stat="identity")

plot_grid(b1, c1,d1,e1 ,
          labels = c("meangvm", "meanUnits", "count of orders", "unit sum","sum of gvm"),
          ncol = 2, nrow = 3)


#a1<-ggplot(order_day_group_summary, aes(x=order_day, y=meangvm, fill=factor(order_day))) + geom_bar(position="dodge", stat="identity")
#b1<-ggplot(order_day_group_summary, aes(x=order_day, y=mean, fill=factor(order_day))) + geom_bar(position="dodge", stat="identity")
c1<-ggplot(order_day_group_summary, aes(x=order_day, y=count, fill=factor(order_day))) + geom_bar(position="dodge", stat="identity")
d1<-ggplot(order_day_group_summary, aes(x=order_day, y=sum, fill=factor(order_day))) + geom_bar(position="dodge", stat="identity")
e1<-ggplot(order_day_group_summary, aes(x=order_day, y=sumgvm, fill=factor(order_day))) + geom_bar(position="dodge", stat="identity")

plot_grid( c1,d1,e1 , 
           labels = c("meangvm", "meanUnits", "count of orders", "unit sum","sum of gvm"),
           ncol = 2, nrow = 3)


a1<-ggplot(order_hour_group_summary, aes(x=order_hour, y=meangvm, fill=factor(order_hour))) + geom_bar(position="dodge", stat="identity")
b1<-ggplot(order_hour_group_summary, aes(x=order_hour, y=mean, fill=factor(order_hour))) + geom_bar(position="dodge", stat="identity")
c1<-ggplot(order_hour_group_summary, aes(x=order_hour, y=count, fill=factor(order_hour))) + geom_bar(position="dodge", stat="identity")
d1<-ggplot(order_hour_group_summary, aes(x=order_hour, y=sum, fill=factor(order_hour))) + geom_bar(position="dodge", stat="identity")
e1<-ggplot(order_hour_group_summary, aes(x=order_hour, y=sumgvm, fill=factor(order_hour))) + geom_bar(position="dodge", stat="identity")

plot_grid(a1, b1, c1,d1,e1 , 
          labels = c("meangvm", "meanUnits", "count of orders", "unit sum","sum of gvm"),
          ncol = 2, nrow = 3)

a1<-ggplot(order_week_group_summary, aes(x=order_week, y=order_week, fill=factor(order_week))) + geom_bar(position="dodge", stat="identity")
b1<-ggplot(order_week_group_summary, aes(x=order_week, y=mean, fill=factor(order_week))) + geom_bar(position="dodge", stat="identity")
c1<-ggplot(order_week_group_summary, aes(x=order_week, y=count, fill=factor(order_week))) + geom_bar(position="dodge", stat="identity")
d1<-ggplot(order_week_group_summary, aes(x=order_week, y=sum, fill=factor(order_week))) + geom_bar(position="dodge", stat="identity")
e1<-ggplot(order_week_group_summary, aes(x=order_week, y=sumgvm, fill=factor(order_week))) + geom_bar(position="dodge", stat="identity")

plot_grid(a1, b1, c1,d1,e1 , 
          labels = c("meangvm", "meanUnits", "count of orders", "unit sum","sum of gvm"),
          ncol = 2, nrow = 3)