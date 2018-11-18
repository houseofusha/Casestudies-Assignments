#uber data analysis
#by usha hariram

#reading data file into R
ubersys <- read.csv("Uber Request Data.csv", header = TRUE, stringsAsFactors = FALSE)
View(ubersys)
str(ubersys)

#checking for duplicates
sum(duplicated(ubersys$Request.id))
##no duplicates

#check count of NA values
length(which(is.na(ubersys)))
##NA values present

#variables with corresponding number of NA values
sapply(colnames(ubersys), function(x) length(which(is.na(ubersys[,x]))))

#string manipulation 
library(stringr)

#replacing seperator "/" with "-" 
ubersys$Request.timestamp <- str_replace_all(ubersys$Request.timestamp,"[/]","-")
ubersys$Drop.timestamp <- str_replace_all(ubersys$Drop.timestamp,"[/]","-")

#converting to std. date-time format
ubersys$Request.timestamp <- as.POSIXlt(ubersys$Request.timestamp, format = "%d-%m-%Y %H:%M")
ubersys$Drop.timestamp <- as.POSIXlt(ubersys$Drop.timestamp, format = "%d-%m-%Y %H:%M")

#separating day, hours & minutes
ubersys$req_day <- format(ubersys$Request.timestamp, "%d")
ubersys$req_hours <- format(ubersys$Request.timestamp, "%H")
ubersys$req_min <- format(ubersys$Request.timestamp, "%M")

#extracting drop hours & minutes
ubersys$drop_hours <- format(ubersys$Drop.timestamp, "%H")
ubersys$drop_mins <- format(ubersys$Drop.timestamp, "%M")

#calculating travel time
ubersys$trip_time = as.numeric(ubersys$Drop.timestamp-ubersys$Request.timestamp)

###

#backup file for tableau
write.csv(ubersys, "ubersys.csv")



#plotting data
library(ggplot2)

#Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; 
#identify the most problematic types of requests (city to airport / airport to city etc.) and 
#the time slots (early mornings, late evenings etc.) using plots

#plotting frequency of requests
ggplot(ubersys, aes(x = ubersys$Status)) + geom_bar(stat = "count")

#plotting freqency of more problimatic request(city to airport vs airport to city)
ggplot(ubersys, aes(x = ubersys$Status, fill = ubersys$Pickup.point)) + geom_bar(stat = "count", position = "fill")
ggplot(ubersys,aes(x=factor(req_hours),fill=factor(Pickup.point)))+geom_bar(position = "dodge")+facet_wrap(~Status)

#most busy timeslots in the day/night
ggplot(ubersys, aes(x = req_hours)) + geom_histogram(stat = "count")

#timeslots when cabs are unavailable or cancelled
ggplot(ubersys, aes(x = req_hours, fill = ubersys$Status)) + geom_bar(position = "fill")
##high cancellation from 4:00am to 10:00am
##unavailability of cars from 5:00pm to 12:00am

#plot of demand spread across the day
ggplot(ubersys, aes(x = req_hours, fill = ubersys$Pickup.point)) + geom_bar(position = "dodge")

#gap between supply and demand. 
#demand is the requests made at the airport & city
#supply is the number of trips completed from airport to the city & city to airport.

#gap of demand and supply at airport
demand_airport <- subset(ubersys,ubersys$Pickup.point == "Airport")
demand1 <- nrow(demand_airport)
supply_airport <- subset(demand_airport,demand_airport$Status == "Trip Completed")
supply1 <- nrow(supply_airport)
gap1 <- demand1 - supply1
gap1

#gap of demand and supply at city
demand_city <- subset(ubersys,ubersys$Pickup.point == "City")
demand2 <- nrow(demand_city)
supply_city <- subset(demand_city,demand_city$Status == "Trip Completed")
supply2 <- nrow(supply_city)
gap2 <- demand2 - supply2
gap2


#time slots when the highest gap exists
#demand at airport
da <- ggplot(demand_airport, aes(x = req_hours)) + geom_bar() + labs(x = "demand at airport on req hours")
da
#supply at airport
sa <- ggplot(supply_airport, aes(x = req_hours)) + geom_bar() + labs(x = "supply at airport on req hours")
sa
#demand at city
dc <- ggplot(demand_city, aes(x = req_hours)) + geom_bar() + labs(x = "demand at city on req hours")
dc
#supply at city
sc <- ggplot(supply_city, aes(x = req_hours)) + geom_bar() + labs(x = "supply at city on req hours")
sc

library(grid)
library(gridExtra)
grid.arrange(da, sa, dc, sc, nrow = 2, ncol = 2)


#types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

ggplot(demand_airport, aes(x = as.factor(req_hours),fill = Status))+geom_bar() + labs(x = "Request Hour", y = "Count", title = "Demand at Airport")
ggplot(demand_city, aes(x = as.factor(req_hours),fill = Status))+geom_bar() + labs(x = "Request Hour", y = "Count", title = "Demand at City")


