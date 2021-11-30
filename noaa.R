library(rnoaa)
library(dplyr)
stations = ghcnd_stations(refresh = FALSE)

dvortsov = data.frame(id="Dvorts",
                          latitude = c(59.568060),
                          longitude= c(30.108330))


station_list = meteo_nearby_stations(lat_lon_df = dvortsov, 
                                     station_data = stations,
                      radius = 150, var = c("PRCP"),
                      year_min = 2008, year_max = 2019)


station_list =station_list[[1]] 
station_list = station_list %>% filter(name %in% c("BELOGORKA", "PETROKREPOST"))


one_station = meteo_tidy_ghcnd(
  "RSM00027719")
two_station = meteo_tidy_ghcnd("RSM00026072")

one_station = one_station %>% select(id,date, prcp, tavg)
two_station = two_station %>% select(id,date, prcp, tavg)

all_data = rbind(one_station, two_station)

all_data = all_data %>% mutate(tavg = tavg /10, prcp = prcp /10)

write.csv(stations, "station_data.csv")
