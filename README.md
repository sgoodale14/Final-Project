# Final-Project
Analyzing Data with R (edX)
#install and load libraries
install.packages("rlang")
install.packages("tidymodels")
library(tidymodels)
library(tidyverse)
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz"
download.file(url, destfile = "noaa-weather-sample-data.tar.gz")
untar("noaa-weather-sample-data.tar.gz",tar = "internal")
#extract and read data
sub_weather <- read_csv("noaa-weather-sample-data/jfk_weather_sample.csv")
head(sub_weather)
#select subset of columns
weather_subset <-sub_weather %>% select(c(HOURLYRelativeHumidity,HOURLYDRYBULBTEMPF, HOURLYPrecip, HOURLYWindSpeed, HOURLYStationPressure))
#show first 10 rows
head(weather_subset,10)
#inspect unique values
unique(weather_subset$HOURLYPrecip)
#replace T with 0.0 and remove "s" suffix from numerals
weather_subset$HOURLYPrecip[weather_subset$HOURLYPrecip=="T"]<-"0.0"
weather_subset$HOURLYPrecip <-str_remove(weather_subset$HOURLYPrecip,pattern="s$)
unique(weather_subset$HOURLYPrecip)
#verify replacement
glimpse(weather_subset)
#rename columns
weather_cleaned <- weather_subset %>%
+     rename(relative_humidity = HOURLYRelativeHumidity)%>%
+     rename(dry_bulb_temp_f = HOURLYDRYBULBTEMPF)%>%
+     rename(precip = HOURLYPrecip)%>%
+     rename(wind_speed = HOURLYWindSpeed)%>%
+     rename(station_pressure = HOURLYStationPressure)
> weather_cleaned
> #histograms
> gplot(data=weather_cleaned,aes(relative_humidity))+geom_histogram()
> ggplot(data=weather_cleaned,aes(dry_bulb_temp_f))+geom_histogram()
> ggplot(data=weather_cleaned,aes(precip))+(stat_count(width = .5))
> ggplot(data=weather_cleaned,aes(wind_speed))+geom_histogram()
> ggplot(data=weather_cleaned,aes(station_pressure))+geom_histogram()
> #boxplots of data
> boxplot(weather_cleaned$relative_humidity)
> boxplot(weather_cleaned$dry_bulb_temp_f)
> boxplot(weather_cleaned$precip)
> boxplot(weather_cleaned$wind_speed)
> boxplot(weather_cleaned$station_pressure)
#plot using precip as target variable
plot(precip ~ station_pressure+wind_speed+relative_humidity+dry_bulb_temp_f, weather_cleaned)
