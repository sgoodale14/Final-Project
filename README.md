# Final-Project
edX: Analyzing Data with R
install.packages("rlang")
install.packages("tidymodels")
library(tidymodels)
library(tidyverse)
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz"
download.file(url, destfile = "noaa-weather-sample-data.tar.gz")
untar("noaa-weather-sample-data.tar.gz",tar = "internal")
noaa_weather <- read_csv( "noaa-weather-sample-data/jfk_weather_sample.csv")
head(noaa_weather)
glimpse(noaa_weather)
#select five columns and save as new variable
sub_noaa <- noaa_weather %>% select(HOURLYRelativeHumidity, HOURLYDRYBULBTEMPF, HOURLYPrecip, HOURLYWindSpeed, HOURLYStationPressure)
#inspect
unique(sub_noaa$HOURLYPrecip)
#Replace T values with 0s and remove -s suffice
df <- mutate(sub_noaa, HOURLYPrecip = replace(sub_noaa$HOURLYPrecip, sub_noaa$HOURLYPrecip == "T", "0.0") )
sub_noaa_df <- mutate(df, HOURLYPrecip = str_remove(df$HOURLYPrecip, "s$"))
unique(sub_noaa_df$HOURLYPrecip)
#inspect new variable
glimpse(sub_noaa_df)
#Make HOURLYprecip to numeric datatype 
sub_noaa_df$HOURLYPrecip <- as.numeric(sub_noaa_df$HOURLYPrecip)
glimpse(sub_noaa_df)
#Rename columns
df_noaa <- sub_noaa_df %>% rename(relative_humidity = HOURLYRelativeHumidity) %>% rename(dry_bulb_temp_f = HOURLYDRYBULBTEMPF)%>%
rename(precip = HOURLYPrecip) %>% rename(wind_speed = HOURLYWindSpeed) %>% rename (station_pressure = HOURLYStationPressure)
head(df_noaa)
#split data into training and testing
set.seed(1234)
noaa_split <- initial_split(df_noaa, prop = 4/5)
#plot histograms
train_data <- training(noaa_split)
test_data <- testing(noaa_split)
