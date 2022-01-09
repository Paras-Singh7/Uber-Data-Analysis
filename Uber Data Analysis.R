#Importing libraries
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(RColorBrewer)

#Importing dataset
apr_data <- read.csv("/content/uber-raw-data-apr14.csv")
may_data <- read.csv("/content/uber-raw-data-may14.csv")
jun_data <- read.csv("/content/uber-raw-data-jun14.csv")
jul_data <- read.csv("/content/uber-raw-data-jul14.csv")
aug_data <- read.csv("/content/uber-raw-data-aug14.csv")
sep_data <- read.csv("/content/uber-raw-data-sep14.csv")

#Preprocessing
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

hour_data <- data_2014 %>% group_by(hour) %>% dplyr::summarize(Total = n())

fig <- function(width, heigth){
     options(repr.plot.width = width, repr.plot.height = heigth)
}

fig(10,10)
ggplot(hour_data, aes(hour, Total)) + 
geom_bar(stat = "identity", fill = "#00FF7F", color = "black") +
ggtitle("Trips Every Hour") + 
theme_bw() +
theme(legend.position = "none",text = element_text(size = 20)) + 
scale_y_continuous(labels = comma)

month_hour <- data_2014 %>% group_by(month, hour) %>% dplyr::summarize(Total = n())

fig(10,10)
ggplot(month_hour, aes(hour, Total, fill = month)) +
geom_bar(stat = "identity") +
ggtitle("Trips by Hour and Month") + 
theme_bw() + 
theme(text = element_text(size = 20)) + 
scale_fill_manual(values = brewer.pal(n = 7, name = "YlOrRd"))

day_group <- data_2014 %>% group_by(day) %>% dplyr::summarize(Total = n())

fig(10,10)
ggplot(day_group, aes(day,Total)) + 
geom_bar(stat = "identity", fill = "#00ff7f", color = 'black') + 
ggtitle("Trips Every Day") +
theme_bw() + 
theme(legend.position = "none", text = element_text(size = 20)) + 
scale_y_continuous(labels = comma)

day_month_group <- data_2014 %>% group_by(month, day) %>% dplyr::summarize(Total = n())
fig(10,10)
ggplot(day_month_group, aes(day, Total, fill=month)) +
geom_bar(stat = "identity") +
ggtitle("Trips by Day and Month") +
theme_bw() + 
theme(text = element_text(size = 20)) +
scale_y_continuous(labels = comma) + 
scale_fill_manual(values = brewer.pal(n = 7, name = "BuPu"))

month_group <- data_2014 %>% group_by(month) %>% dplyr::summarize(Total = n())

fig(8,8)
ggplot(month_group, aes(month, Total, fill=month))+
geom_bar(stat = "identity", color = 'black') +
ggtitle("Trips by Month") +
theme_bw() +
theme(legend.position = "none", text = element_text(size = 15)) +
scale_y_continuous(labels = comma) + 
scale_fill_manual(values = brewer.pal(n = 6, name="YlOrRd"))

month_weekday <- data_2014 %>% group_by(month, dayofweek) %>% dplyr::summarize(Total = n())
fig(10,10)
ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
geom_bar(stat = "identity", position ="dodge", color="black") +
ggtitle("Trips by Day and Month") +
theme_bw() + 
theme(text = element_text(size = 20)) + 
scale_y_continuous(labels = comma) + 
scale_fill_manual(values = brewer.pal(n=7, name = "GnBu"))

fig(8,8)
ggplot(data_2014, aes(Base)) +
geom_bar(fill = "#b314fc", color = 'black') +
scale_y_continuous(labels = comma) +
ggtitle("Trips by Bases") + 
theme_bw() +
theme(text = element_text(size = 20))

fig(10,10)
ggplot(data_2014, aes(Base, fill = month)) + 
geom_bar(position = "dodge", color='black') +
scale_y_continuous(labels = comma) +
ggtitle("Trips by Bases and Month") +
theme_bw() +
theme(text = element_text(size = 20)) + 
scale_fill_manual(values = brewer.pal(n = 6, name = 'BrBG'))

fig(10,10)
ggplot(data_2014, aes(Base, fill=dayofweek)) +
geom_bar(position = "dodge", color ="black") + 
scale_y_continuous(labels = comma) +
ggtitle("Trips by Bases and DayofWeek") +
theme_bw() +
theme(text = element_text(size = 20)) +
scale_fill_manual(values = brewer.pal(n=7, name="GnBu"))

day_and_hour <- data_2014 %>% group_by(day, hour) %>% dplyr::summarize(Total = n())

fig(12,12)
ggplot(day_and_hour, aes(day, hour, fill = Total)) +
geom_tile(color = "white")+
ggtitle("Heat Map by Hour and Day") +
theme(text = element_text(size=20)) 

fig(12,12)
ggplot(day_month_group, aes(day, month, fill=Total)) +
geom_tile(color = "white")+
ggtitle("Heat Map by Month and Day")+
theme(text = element_text(size =20))

fig(10,10)
ggplot(month_weekday, aes(dayofweek, month, fill=Total)) +
geom_tile(color = "white") +
ggtitle("Heat Map by Month and Day of Week") +
theme(text = element_text(size = 20))

month_base <- data_2014 %>% group_by(Base, month) %>% dplyr::summarize(Total = n())
dayOfweek_bases <- data_2014 %>% group_by(Base, dayofweek) %>% dplyr::summarize(Total = n())

fig(10,10)
ggplot(month_base, aes(Base, month, fill = Total)) +
geom_tile(color = "white") +
ggtitle("Heat Map by Month and Bases") + 
theme(text = element_text(size = 20))

fig(10,10)
ggplot(dayOfweek_bases, aes(Base, dayofweek, fill=Total)) +
geom_tile(color = "white") + 
ggtitle("Heat Map by Bases and Day of Week") +
theme(text = element_text(size = 20))

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

fig(15,10)
ggplot(data_2014, aes(x = Lon, y = Lat)) +
geom_point(size=1, color = "blue") +
scale_x_continuous(limits =c(min_long, max_long)) +
scale_y_continuous(limits = c(min_lat, max_lat)) +
theme_map() +
theme(text = element_text(size = 20)) +
ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)") +

fig(15,10)
ggplot(data_2014, aes(x=Lon, y=Lat, color=Base))+
geom_point(size=1)+
scale_x_continuous(limits=c(min_long, max_long))+
scale_y_continuous(limits=c(min_lat, max_lat))+
theme_map()+
theme(text = element_text(size = 20)) +
ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE") 