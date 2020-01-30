####connect to database and load the data
library(RSQLite)
dcon <- dbConnect(SQLite(), dbname = "E:/Course/605 R for Data Science/Project/project2.db")
dbListTables(dcon)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM taxi14;
")
green_taxi.14 <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM taxi15;
")
green_taxi.15 <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM uber14;
")
uber.14 <- dbFetch(res, -1)
dbClearResult(res)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM uber15;
")
uber.15 <- dbFetch(res, -1)
dbClearResult(res)
dbDisconnect(dcon)
####transfer the date to correct format
uber.14$Date<-strptime(uber.14$`Date/Time`,"%m/%d/%Y %H:%M:%S")
uber.14$day<-as.Date(uber.14$Date,"%Y-%m-%d")

uber.15$Date<-strptime(uber.15$Pickup_date,"%Y-%m-%d %H:%M:%S")
uber.15$day<-as.Date(uber.15$Date,"%Y-%m-%d")

green_taxi.14$Date<-strptime(green_taxi.14$lpep_pickup_datetime,"%Y-%m-%d %H:%M:%S")
green_taxi.14$day<-as.Date(green_taxi.14$Date,"%Y-%m-%d")

green_taxi.15$Date<-strptime(green_taxi.15$lpep_pickup_datetime,"%Y-%m-%d %H:%M:%S")
green_taxi.15$day<-as.Date(green_taxi.15$Date,"%Y-%m-%d")

####get the number for everyday
uber.14.num<-as.data.frame(table(uber.14$day))
colnames(uber.14.num)<-c("date","Num")
uber.14.num$date<-as.Date(uber.14.num$date)

uber.15.num<-as.data.frame(table(uber.15$day))
colnames(uber.15.num)<-c("date","Num")
uber.15.num$date<-as.Date(uber.15.num$date)

greentaxi.14.num<-as.data.frame(table(green_taxi.14$day))
colnames(greentaxi.14.num)<-c("date","Num")
greentaxi.14.num$date<-as.Date(greentaxi.14.num$date)

greentaxi.15.num<-as.data.frame(table(green_taxi.15$day))
colnames(greentaxi.15.num)<-c("date","Num")
greentaxi.15.num$date<-as.Date(greentaxi.15.num$date)

#################################################    New
####draw monthly plot
uber.num<-rbind(uber.14.num,uber.15.num)
green.num<-rbind(greentaxi.14.num,greentaxi.15.num)
uber.num$Type<-rep(c("Uber"),length(uber.num[,1]))
green.num$Type<-rep(c("Taxi"),length(green.num[,1]))

data.201415<-rbind(uber.num,green.num)

library(ggplot2)

ggplot()+
  geom_point(data=data.201415,aes(date,Num,color=Type),size=1)+
  geom_smooth(data = uber.num,aes(date,Num),method = 'loess',color="#0072B2",se=FALSE)+
  geom_smooth(data = green.num,aes(date,Num),method = 'loess',color="#000000",se=FALSE)+
  ylim(0,120000)+
  labs(x = "Date", y="Number of Pickups", title = "Uber vs. Taxi Pickups") +
  theme(
    axis.text.x = element_text(hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    title = element_text(size = 13))

##c("#CC79A7", "#D55E00", "#56B4E9", "#F0E442","#009E73", "#0072B2", "#999999", "#E69F00")
##粉色，红色，蓝色，黄色，绿色，深蓝色，灰色，橘黄色
####draw the day plot
uberday.14<-as.data.frame(table(format(uber.14$Date,"%H")))
uberday.15<-as.data.frame(table(format(uber.15$Date,"%H")))
greenday.14<-as.data.frame(table(format(green_taxi.14$Date,"%H")))
greenday.15<-as.data.frame(table(format(green_taxi.15$Date,"%H")))
colnames(uberday.14)<-c("Time","Num")
colnames(uberday.15)<-c("Time","Num")
colnames(greenday.14)<-c("Time","Num")
colnames(greenday.15)<-c("Time","Num")
uberday.14$Num <-uberday.14$Num/6
uberday.15$Num<-uberday.15$Num/6
greenday.14$Num<-greenday.14$Num/9
greenday.15$Num<-greenday.15$Num/6

####Uber 2014 vs 2015
a<-rep(0:23,each=2)
b<-rep(c("2014","2015"),24)
c<-numeric(48)
for (i in 1:24) {
  n<-(i-1)*2+1
  c[n]<-uberday.14[i,2]
  c[n+1]<-uberday.15[i,2]
}
uber<-as.data.frame(cbind(a,b,c))
uber$b<-as.character(uber$b)
uber$c<-as.numeric(as.character(uber$c))
colnames(uber)<-c("Hour","Year","Num")
ggplot(data=uber,mapping = aes(x=factor(Hour,levels = c(0:23)),y=Num,fill=Year)) +
  geom_bar(stat = 'identity', position = 'dodge')+
  labs(x = "Time", y="Number of Customers", title = "2014 vs. 2015 Uber Pickups")+
  theme(axis.text.x = element_text(hjust = 1, size = 5),
        axis.text.y = element_text(size = 8),
        title = element_text(size = 13))
 

####Taxi 2014 vs 2015
a<-rep(0:23,each=2)
b<-rep(c("2014","2015"),24)
c<-numeric(48)
for (i in 1:24) {
  n<-(i-1)*2+1
  c[n]<-greenday.14[i,2]
  c[n+1]<-greenday.15[i,2]
}
green<-as.data.frame(cbind(a,b,c))
green$b<-as.character(green$b)
green$c<-as.numeric(as.character(green$c))
colnames(green)<-c("Hour","Year","Num")
ggplot(data=green,mapping = aes(x=factor(Hour,levels = c(0:23)),y=Num,fill=Year)) +
  geom_bar(stat = 'identity', position = 'dodge')+
  labs(x = "Time", y="Number of Customers", title = "2014 vs. 2015 Taxi Pickups")+
  theme(axis.text.x = element_text(hjust = 1, size = 5),
        axis.text.y = element_text(size = 8),
        title = element_text(size = 13))


#### 2014 Uber vs Green
a<-rep(0:23,each=2)
b<-rep(c("Uber","Taxi"),24)
c<-numeric(48)
for (i in 1:24) {
  n<-(i-1)*2+1
  c[n]<-uberday.14[i,2]
  c[n+1]<-greenday.14[i,2]
}
data.2014<-as.data.frame(cbind(a,b,c))
data.2014$b<-as.character(data.2014$b)
data.2014$c<-as.numeric(as.character(data.2014$c))
colnames(data.2014)<-c("Hour","Type","Num")
ggplot(data=data.2014,mapping = aes(x=factor(Hour,levels = c(0:23)),y=Num,fill=Type)) +
  geom_bar(stat = 'identity', position = 'dodge')+
  labs(x = "Time", y="Number of Customers", title = "2014 Uber vs. Taxi Pickups")+
  theme(axis.text.x = element_text(hjust = 1, size = 5),
        axis.text.y = element_text(size = 8),
        title = element_text(size = 13))




####2015 uber vs taxi
a<-rep(0:23,each=2)
b<-rep(c("Uber","Taxi"),24)
c<-numeric(48)
for (i in 1:24) {
  n<-(i-1)*2+1
  c[n]<-uberday.15[i,2]
  c[n+1]<-greenday.15[i,2]
}
data.2015<-as.data.frame(cbind(a,b,c))
data.2015$b<-as.character(data.2015$b)
data.2015$c<-as.numeric(as.character(data.2015$c))
colnames(data.2015)<-c("Hour","Type","Num")
ggplot(data=data.2015,mapping = aes(x=factor(Hour,levels = c(0:23)),y=Num,fill=Type)) +
  geom_bar(stat = 'identity', position = 'dodge')+
  labs(x = "Time", y="Number of Customers", title = "2015 Uber vs. Taxi Pickups")+
  theme(axis.text.x = element_text(hjust = 1, size = 5),
        axis.text.y = element_text(size = 8),
        title = element_text(size = 13))


####killer plot
conn_weather <- dbConnect(SQLite(), "E://Course//605 R for Data Science//Project//weather.db")
res<- dbSendQuery(conn=weather, "
                SELECT  Date, Weather, CASE  
                                            WHEN Time = '6:51 am' THEN '06'
                                            WHEN Time = '7:51 am' THEN '07'
                                            WHEN Time = '8:51 am' THEN '08'
                                            WHEN Time = '9:51 am' THEN '09'
                                            WHEN Time = '10:51 am' THEN '10'
                                            WHEN Time = '11:51 am' THEN '11'
                                            WHEN Time = '12:51 pm' THEN '12'
                                            WHEN Time = '1:51 pm' THEN '13'
                                            WHEN Time = '2:51 pm' THEN '14'
                                            WHEN Time = '3:51 pm' THEN '15'
                                            WHEN Time = '4:51 pm' THEN '16'
                                            WHEN Time = '5:51 pm' THEN '17'
                                            WHEN Time = '6:51 pm' THEN '18'
                                            WHEN Time = '7:51 pm' THEN '19'
                                            WHEN Time = '8:51 pm' THEN '20'
                                            WHEN Time = '9:51 pm' THEN '21'
                                            WHEN Time = '10:51 pm' THEN '22'
                                        ELSE 'Invalid' END as Time
                FROM weather
                WHERE Time in (
                '6:51 am', '7:51 am', '8:51 am', '9:51 am', 
                '10:51 am', '11:51 am', '12:51 pm', '1:51 pm', 
                '2:51 pm', '3:51 pm', '4:51 pm', '5:51 pm', 
                '6:51 pm', '7:51 pm', '8:51 pm', '9:51 pm', '10:51 pm') AND
                Weather in (
                'Sunny.', 
                'Clear',
                'Overcast',
                'Passing clouds.',
                'Fog.',
                'Partly sunny.',
                'Mostly cloudy.',
                'Scattered clouds.',
                'Low clouds.',
                'Broken clouds.'
                )
                            ")
weather <- dbFetch(res, -1) 
dbClearResult(res)
dbDisconnect(dcon)