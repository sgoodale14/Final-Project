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
#convert to numeric
weather_subset$HOURLYPrecip <- as.numeric(weather_subset$HOURLYPrecip)
weather_subset$HOURLYWindSpeed <- as.numeric(weather_subset$HOURLYWindSpeed)
weather_subset$HOURLYRelativeHumidity <- as.numeric(weather_subset$HOURLYRelativeHumidity)
weather_subset$HOURLYDRYBULBTEMPF <- as.numeric(weather_subset$HOURLYDRYBULBTEMPF)
weather_subset$HOURLYStationPressure <- as.numeric(weather_subset$HOURLYStationPressure)
glimpse(weather_subset)
#rename columns
weather_cleaned <- weather_subset %>%
+     rename(relative_humidity = HOURLYRelativeHumidity)%>%
+     rename(dry_bulb_temp_f = HOURLYDRYBULBTEMPF)%>%
+     rename(precip = HOURLYPrecip)%>%
+     rename(wind_speed = HOURLYWindSpeed)%>%
+     weather_cleaned
> weather_cleaned
> #split data into training set
> set.seed(1234)
> weather_split <- initial_split(weather_cleaned, prop = 4/5)
> train_data <- training(weather_split)
> test_data <- testing(weather_split)
> #histograms
> ggplot(data=weather_cleaned,aes(relative_humidity))+geom_histogram()
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
#run linear regression
 lm.model=lm(precip ~ relative_humidity,data=weather_cleaned)
 summary(lm.model)
lm.model=lm(precip ~ dry_bulb_temp_f,data=weather_cleaned)
summary(lm.model)
lm.model=lm(precip ~ wind_speed,data=weather_cleaned)
summary(lm.model)
lm.model=lm(precip ~ station_pressure,data=weather_cleaned)
summary(lm.model)
