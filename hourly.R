#http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=26892&Year=2017&Month=12&Day=7&timeframe=1&submit=Download+Data


library(purrr)
library(magrittr)
library(dplyr)
library(readr)
library(tidyverse)
library(viridis)
library(gridExtra)
library(animation)
library(ggplot2)
#timeframe 1 = hourly 1 mois a la fois
#timeframe 2 = daily, 1 an a la fois
#timeframe 3 = monthly (tout l'historique)

dailyskip <- 25 
hourlyskip <- 16
## import weather data, save clean data to csv.
download.file("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=26892&Year=2017&Month=12&timeframe=1&submit=Download+Data",
              destfile="data/pouet.csv")
year <- rep( seq(from = 2005, to = 2017), each= 12)
month <- rep( seq (1:12), times = length(year)/12)
liste <- tibble(year, month)

stationID = "26892"

map2(liste$year,liste$month, 
     ~download.file(
       paste0("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=26892&Year=", .x, "&Month=", .y, "&timeframe=1&submit=Download+Data"),
       destfile=paste0("data/hourly_26892_",.x , "_", .y , ".csv")
       ))


year <- rep( seq(from = 2005, to = 2017), each= 12)
month <- rep( seq (1:12), times = length(year)/12)
liste <- tibble(year, month) 
hourly <- bind_rows(
  map2(liste$year, liste$month,
      ~  read_csv(paste0("data/hourly_26892_", .x,"_", .y, ".csv"), skip = hourlyskip)   %>% 
        rename(date_time = "Date/Time", 
               year = Year,
               month = Month,
               day = Day,
               time = "Time",
               data_quality = "Data Quality",
               temp = "Temp (°C)",
               temp_flag = "Temp Flag",
               dew_point_temp = "Dew Point Temp (°C)",
               dew_point_temp_flag = "Dew Point Temp Flag",
               rel_hum = "Rel Hum (%)",
               rel_hum_flag = "Rel Hum Flag",
               wind_dir = "Wind Dir (10s deg)",
               wind_dir_flag = "Wind Dir Flag",
               wind_speed= "Wind Spd (km/h)",
               wind_speed_flag = "Wind Spd Flag",
               visibility = "Visibility (km)",
               visibility_flag = "Visibility Flag" ,
               stn_press = "Stn Press (kPa)",
               stn_press_flag = "Stn Press Flag",
               hmdx = "Hmdx",
               hmdx_flag = "Hmdx Flag",
               wind_chill = "Wind Chill",
               wind_chill_flag = "Wind Chill Flag",
               weather = "Weather") %>%
        mutate_at( vars(ends_with("flag")),             funs(ifelse(as.character(.)=="TRUE", "T", as.character(.) ))) %>%
        #mutate(hmdx = ifelse( hmdx == "<NA>", NA, hmdx)) %>%
        mutate(month = as.numeric(month)) %>%
        mutate_at(vars(temp, dew_point_temp, rel_hum, wind_dir, wind_speed, visibility, stn_press, hmdx, wind_chill),
                  funs( ifelse( . == "<NA>", NA, as.numeric(.))))
  ))


hourly_count <- hourly %>% 
  summarise_at(vars(everything()), funs(sum(!is.na(.))))  %>%
  tidyr::gather(everything(), 
                key="key", 
                value = "value")


#daily
year <- rep( seq(from = 1992, to = 2017))
liste <- tibble(year)
map(liste$year,
     ~download.file(
       paste0("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=26892&Year=", .x, "&timeframe=2&submit=Download+Data"), #, "&Month=", .y
       destfile=paste0("data/daily_26892_",.x ,  ".csv") #"_", .y ,
     ))


year <- rep( seq(from = 2015, to = 2015))
liste <- tibble(year)
View(head(daily))

daily <- bind_rows(
  map(liste$year,
                    ~  read_csv(paste0("data/daily_26892_", .x, ".csv"), skip = dailyskip)  %>% 
                       rename(date_time = "Date/Time", 
                              year = Year,
                              month = Month,
                              day = Day,
                              min_temp = "Min Temp (°C)",
                              max_temp = "Max Temp (°C)",
                              data_quality = "Data Quality",
                              max_temp_flag = "Max Temp Flag",
                              min_temp_flag = "Min Temp Flag" ,
                              mean_temp = "Mean Temp (°C)" ,
                              mean_temp_flag = "Mean Temp Flag" ,
                              heat_deg_days = "Heat Deg Days (°C)"   ,
                              heat_deg_days_flag = "Heat Deg Days Flag",
                              cool_deg_day = "Cool Deg Days (°C)" ,
                              cool_deg_days_flag =  "Cool Deg Days Flag",
                              total_rain = "Total Rain (mm)",
                              total_rain_flag = "Total Rain Flag" ,
                              total_snow = "Total Snow (cm)",
                              total_snow_flag = "Total Snow Flag",
                              total_precip = "Total Precip (mm)",
                              total_precip_flag = "Total Precip Flag",
                              snow_on_grnd = "Snow on Grnd (cm)",
                              snow_on_grnd_flag = "Snow on Grnd Flag",
                              dir_of_max_gust = "Dir of Max Gust (10s deg)",
                              dir_of_max_gust_flag = "Dir of Max Gust Flag",
                              speed_of_max_gust = "Spd of Max Gust (km/h)",
                              speed_of_max_gust_flag = "Spd of Max Gust Flag") %>%
  mutate_at( vars(ends_with("flag")),             funs(ifelse(as.character(.)=="TRUE", "T", as.character(.) ))) %>%
    mutate(speed_of_max_gust = ifelse(speed_of_max_gust == "<31",0, as.numeric(speed_of_max_gust)))
  ))
                              
write_csv(daily, "data/daily.csv")                              
ggplot(aes(x=date_time), data= daily %>% filter(year >= 2008)  )+ geom_ribbon(aes( ymax= max_temp, ymin= min_temp), fill = "grey80") + geom_line(aes(y=max_temp)) + geom_line(aes(y=min_temp))

ggplot(aes(x=date_time), data= daily %>% filter(year >= 2008))+ geom_ribbon(aes( ymax=speed_of_max_gust, ymin= 0), fill = "grey20")
ggplot(aes(x=date_time), data= daily %>% filter(year >= 2008))+ geom_ribbon(aes( ymax= total_precip, ymin= 0), fill = "grey20")
ggplot(aes(x=date_time), data= daily %>% filter(year >= 2008))+ geom_ribbon(aes( ymax= total_snow, ymin= 0), fill = "grey20") 
daily_count <-
  daily %>% 
  summarise_at(vars(everything()), funs(sum(!is.na(.)))) %>% #GOOD : max_temp, min_temp, mean_temp, total_precip
  tidyr::gather(everything(), key="key", value = "value")

