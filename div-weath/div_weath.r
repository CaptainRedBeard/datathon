##DATATHON!!!!!

##Loading Necessary Libraries/Packages
print("Loading Libraries...")
suppressMessages(library(ggplot2))
suppressMessages(library(lattice))
suppressMessages(library(car))
suppressMessages(library(caret))
suppressMessages(library(e1071))
suppressMessages(library(gplots))
suppressMessages(library(ROCR))
suppressMessages(library(MASS))
suppressMessages(library(nnet))
suppressMessages(library(plyr))
suppressMessages(library(lubridate))
suppressMessages(library(splines))
suppressMessages(library(mgcv))
suppressMessages(library(ISOweek))
suppressMessages(library(tigerstat))
suppressMessages(library(devtools))
suppressMessages(library(ggmap))
suppressMessages(library(rCharts))

##Loading in Data
print("Pulling in Raw Data")
weather_2015 <- data.frame(read.csv("~/Documents/HackAThon/NOAA/2015chihourly.csv", header = F, stringsAsFactor = F))
weather_2014 <- data.frame(read.csv("~/Documents/HackAThon/NOAA/2014chihourly.csv", header = F, stringsAsFactor = F))

weather <- rbind(weather_2014, weather_2015)

colnames(weather) <- c("WBAN","Date","Time","StationType","SkyCondition","SkyConditionFlag","Visibility",
	"VisibilityFlag","WeatherType","WeatherTypeFlag","DryBulbFarenheit","DryBulbFarenheitFlag","DryBulbCelsius",
	"DryBulbCelsiusFlag","WetBulbFarenheit","WetBulbFarenheitFlag","WetBulbCelsius","WetBulbCelsiusFlag",
	"DewPointFarenheit","DewPointFarenheitFlag","DewPointCelsius","DewPointCelsiusFlag","RelativeHumidity",
	"RelativeHumidityFlag","WindSpeed","WindSpeedFlag","WindDirection","WindDirectionFlag","ValueForWindCharacter",
	"ValueForWindCharacterFlag","StationPressure","StationPressureFlag","PressureTendency","PressureTendencyFlag",
	"PressureChange","PressureChangeFlag","SeaLevelPressure","SeaLevelPressureFlag","RecordType","RecordTypeFlag",
	"HourlyPrecip","HourlyPrecipFlag","Altimeter","AltimeterFlag")

divvy_stations <- data.frame(read.csv("~/Documents/HackAThon/Divvy/Divvy_Stations_2015.csv", header = T, stringsAsFactor = F))
divvy_data <- data.frame(read.csv("~/Documents/HackAThon/Divvy/all_divvy_trips.csv", header = T, stringsAsFactor = F))

divvy_data2 <- merge(divvy_data, divvy_stations, by.x = 'from_station_id', by.y = 'id', all.x=T)
colnames(divvy_data2)[13] <- "from_name"
colnames(divvy_data2)[14] <- "from_latitude"
colnames(divvy_data2)[15] <- "from_longitude"
colnames(divvy_data2)[16] <- "from_dpcapacity"
colnames(divvy_data2)[17] <- "from_landmark"

divvy_data3 <- merge(divvy_data2, divvy_stations, by.x = 'to_station_id', by.y = 'id', all.x=T)
colnames(divvy_data3)[18] <- "to_name"
colnames(divvy_data3)[19] <- "to_latitude"
colnames(divvy_data3)[20] <- "to_longitude"
colnames(divvy_data3)[21] <- "to_dpcapacity"
colnames(divvy_data3)[22] <- "to_landmark"

divvy_data3$start_date <- as.Date(divvy_data3$starttime, format='%m/%d/%Y')
divvy_data3$stop_date <- as.Date(divvy_data3$stoptime, format='%m/%d/%Y')
divvy_data3$start_time <- strptime(divvy_data3$starttime, "%m/%d/%Y %H:%M")
divvy_data3$start_time <- format(divvy_data3$start_time, "%H:%M")
divvy_data3$stop_time <- strptime(divvy_data3$stoptime, "%m/%d/%Y %H:%M")
divvy_data3$stop_time <- format(divvy_data3$stop_time, "%H:%M")

divvy_final <- divvy_data3


p_divvy <- ggmap(get_map("Chicago", maptype = 'satellite')) + scale_x_continuous(limits = c(-87.75, -87.5), 
	expand = c(0,0)) + scale_y_continuous(limits = c(41.75, 42.05), expand = c(0,0)) +
	data =divvy_final)

print(p_divvy)

divvy_final2 <- na.omit(divvy_final)

divvy_final2$birthyear <- as.numeric(divvy_final2$birthyear, rm.na = T)
divvy_final2$age <- 2015 - divvy_final2$birthyear
divvy_final2$tripduration <- as.numeric(divvy_final2$tripduration, rm.na= T)

divvy_final2$trip_count <- 1
divvy_final2$trip_dur_min <- divvy_final2$tripduration/60

divvy_final3 <- subset(divvy_final2, !(divvy_final2$usertype %in% "Dependent"))
divvy_final4 <- subset(divvy_final3, !(divvy_final3$usertype %in% "Customer" & divvy_final3$gender %in% c("Female", "Male")))

nrow(divvy_final4)

nrow(divvy_final4)
	
divvy_final4$rounded_min <- round(divvy_final4$trip_dur_min,0)

d_avg_trip_gender_age <- ddply(divvy_final4, c('gender', 'age'), summarize, trip_dur = mean(trip_dur_min), trip_count = sum(trip_count))
d_avg_trip_gender <- ddply(divvy_final4, c('gender'), summarize, trip_dur = mean(trip_dur_min), trip_count = sum(trip_count))
d_avg_trip_gender_user <- ddply(divvy_final4, c('gender', 'usertype'), summarize, trip_dur = mean(trip_dur_min), trip_count = sum(trip_count))

d_hist_gend_age <- ddply(divvy_final4, c('gender', 'rounded_min'), summarize, frequency = sum(trip_count))
d_hist_gend_age2 <- subset(d_hist_gend_age, d_hist_gend_age$rounded_min <= 30)
d_hist_gend_age2 <- d_hist_gend_age2[order(d_hist_gend_age2$rounded_min),]
p_hist_gend_age <- nPlot(x="rounded_min", y="frequency", data = d_hist_gend_age2, type="multiBarChart", group="gender")

p_hist_gend_age$set(title = "Trip Count by Duration & Gender")
p_hist_gend_age$xAxis(axisLabel ='Trip Duration')  
p_hist_gend_age$yAxis(axisLabel = "Frequency", width = 63)

p_hist_gend_age$save('~/Documents/HackAThon/Divvy_histogram_gend_triplength.html')





