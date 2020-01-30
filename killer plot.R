library(grid)
library(dplyr)
weather <- dbConnect(SQLite(), "E://Course//605 R for Data Science//Project//weather.db")
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



uber.14$`Date/Time` <-strptime(uber.14$`Date/Time`,"%m/%d/%Y %H:%M:%S")
uber.15$Pickup_date<-strptime(uber.15$Pickup_date,"%Y-%m-%d %H:%M:%S")
green_taxi.14$lpep_pickup_datetime<-strptime(green_taxi.14$lpep_pickup_datetime,"%Y-%m-%d %H:%M:%S")
green_taxi.15$lpep_pickup_datetime<-strptime(green_taxi.15$lpep_pickup_datetime,"%Y-%m-%d %H:%M:%S")


green_taxi<-rbind(green_taxi.14[,c(1:4)],green_taxi.15[,c(1:4)])
colnames(uber.15)<-c("x1","Base","Date/Time","X2","x3")
uber<-rbind(uber.14[,c(1,4)],uber.15[,c(3,2)])


green_taxi$Time <- format(green_taxi$lpep_pickup_datetime, "%H")
green_taxi$Date <- format(green_taxi$lpep_pickup_datetime, "%Y-%m-%d")
green_taxi_and_weather <- merge(green_taxi, weather, by=c('Date', 'Time'))
green_taxi_count <- green_taxi_and_weather %>%
  group_by(Time, Weather) %>%
  summarise(count=n())
data.green<-green_taxi_count%>%
  group_by(Weather)%>%
  summarise(sum(count))


uber$Time <- format(uber$`Date/Time`, "%H")
uber$Date <- format(uber$`Date/Time`, "%Y-%m-%d")
uber_and_weather <- merge(uber, weather, by=c('Date', 'Time'))
uber_count <- uber_and_weather %>%
  group_by(Time, Weather) %>%
  summarise(count=n())
data.uber<-uber_count%>%
  group_by(Weather)%>%
  summarise(sum(count))

data.green<-as.data.frame(data.green)
data.uber<-as.data.frame(data.uber)

grid.newpage()
vp <- viewport(width = 1, height =1)
pushViewport(vp)
grid.rect(gp=gpar(col="gray",fill="#e9e9e9"))
popViewport()
vp <- viewport(width = 0.8, height = 0.8)
pushViewport(vp)
x<-cos(seq(0,1,length.out = 10000)*2*pi)
y<-sin(seq(0,1,length.out = 10000)*2*pi)
grid.polygon(0.5+x*0.5,0.5+y*0.5,gp=gpar(lwd=2.5,col="white",fill="#e9e9e9"))
grid.polygon(0.5+x*0.3,0.5+y*0.3,gp=gpar(lwd=2.5,col="white",fill="#e9e9e9"))
grid.polygon(0.5+x*0.1,0.5+y*0.1,gp=gpar(lwd=2.5,col="white",fill="#e9e9e9"))
n<-8
angle<-pi/n
color<-topo.colors(20)
for (i in 1:12) {
  grid.lines(x=c(0.5,0.5+cos(angle*(i*2))*0.5),y=c(0.5,0.5+sin(angle*(i*2))*0.5),
             gp=gpar(lwd=2.5,col="white"))
}



riot<-data.green[,2]/5500000
rid<-riot*0.4



for (i in 1:n) {
  x<-c(0.5,0.5+rid[i]*cos(angle*(i-1)),0.5+rid[i]*cos(angle*i),0.5)
  y<-c(0.5,0.5+rid[i]*sin(angle*(i-1)),0.5+rid[i]*sin(angle*i),0.5)
  grid.polygon(x,y,gp=gpar(fill=color[i]))
  grid.text(data.green$Weather[i],x=0.5+(0.1+rid[i])*cos(angle*(i-1)),
            y=0.5+(0.1+rid[i])*sin(angle*(i-1)),
            gp=gpar(fontsize=8))
}

riot<-data.uber[,2]/5500000
rid<-riot*0.4



for (i in 1:n) {
  x<-c(0.5,0.5+rid[i]*cos(-angle*(i-1)),0.5+rid[i]*cos(-angle*i),0.5)
  y<-c(0.5,0.5+rid[i]*sin(-angle*(i-1)),0.5+rid[i]*sin(-angle*i),0.5)
  grid.polygon(x,y,gp=gpar(fill=color[i+8]))
  grid.text(data.green$Weather[i],x=0.5+(0.1+rid[i])*cos(-angle*(i-1)),
            y=0.5+(0.1+rid[i])*sin(-angle*(i-1)),
            gp=gpar(fontsize=8))
}

grid.text("Taxi",x=0.1,y=0.9,gp=gpar(fontsize=15))
grid.text("Uber",x=0.1,y=0.1,gp=gpar(fontsize=15))
popViewport()

