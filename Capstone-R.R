install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("ggplot2")


library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)

rm(list=ls())
dir("Data", full.names = T)


df1<- read.csv("202106-divvy-tripdata.csv")
df2<- read.csv("202107-divvy-tripdata.csv")
df3<- read.csv("202108-divvy-tripdata.csv")
df4<- read.csv("202109-divvy-tripdata.csv")
df5<- read.csv("202110-divvy-tripdata.csv")
df6<- read.csv("202111-divvy-tripdata.csv")
df7<- read.csv("202112-divvy-tripdata.csv")
df8<- read.csv("202201-divvy-tripdata.csv")
df9<- read.csv("202202-divvy-tripdata.csv")
df10<- read.csv("202203-divvy-tripdata.csv")
df11<- read.csv("202204-divvy-tripdata.csv")
df12<- read.csv("202205-divvy-tripdata.csv")


bike_rides<- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)


head(df1)
head(df2)
head(df3)
head(df4)
head(df5)
head(df6)
head(df7)
head(df8)


bikes_rides<- janitor::remove_empty(bike_rides, which = c("cols"))
bikes_rides<- janitor::remove_empty(bike_rides, which = c("rows"))


bike_rides$start_hour<- lubridate:: hour(bike_rides$started_at)
bike_rides$end_hour<- lubridate:: hour(bike_rides$ended_at)


bike_rides%>% count(day_of_week)%>%
  ggplot()+ geom_line(aes(x=day_of_week, y=n))+
  labs(title= "Day of the Week VS The Frequency")


colnames(bike_rides)
dim(bike_rides)
head(bike_rides)
str(bike_rides)


bike_rides$date <- as.Date(bike_rides$started_at)
bike_rides$month <- format(as.Date(bike_rides$date), "%m")
bike_rides$day <- format(as.Date(bike_rides$date), "%d")
bike_rides$year <- format(as.Date(bike_rides$date), "%Y")
bike_rides$ride_length_m<- (as.difftime(bike_rides$ended_at, bike_rides$started_at))/60
str(bike_rides)


bike_rides%>%
  group_by(day_of_week)%>%
  summarise(number_of_rides=n())%>%
  ggplot(mapping=aes(x=day_of_week, y=number_of_rides)) + geom_col()


bike_rides%>%
  group_by(member_casual, day_of_week)%>%
  summarise(number_of_rides = n(), average_duration=mean(ride_length_m))%>%
  arrange(member_casual,day_of_week)%>%
  ggplot(aes(x=day_of_week, y=average_duration, fill= member_casual)) + geom_col(position = "dodge")


bike_rides%>%
  group_by(member_casual)%>%
  summarise(rider_count=n())%>%
  ggplot(aes(x=member_casual,y=rider_count, fill=member_casual)) + geom_col()


bike_rides$ride_length<-as.numeric(as.character(bike_rides$ride_length))
bike_rides$month <- as.numeric(bike_rides$month)
bike_rides$day <- as.numeric(bike_rides$day)
is.numeric(bike_rides$ride_length)
is.numeric(bike_rides$ride_month)
is.numeric(bike_rides$ride_day)



bike_rides$ride

filtered_casuals_on_sundays<- filter(bike_rides,member_casual=="casual" & day_of_week==7)
filtered_members_on_sundays<- filter(bike_rides,member_casual=="member" & day_of_week==7)

bike_rides%>%
  group_by(filtered_casuals_on_sundays, filtered_members_on_sundays)%>%
  summarise(rider_count=n())%>%
  ggplot(aes(x=filtered_casuals_on_sundays, y=rider_count)) + geom_col()


ggplot(data=bike_rides) +
  geom_bar(mapping=aes(x=filtered_casuals_on_sundays, y=n))


?fill

