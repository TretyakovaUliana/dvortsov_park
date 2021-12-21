library(tidyverse)
library(lubridate)
library(rnoaa)
library(raster)
library(sp)
library(sf)
library(elevatr)
library(rLandsat)
library(rvest)
library(curl)
library(RStoolbox)
library(RCurl)
library(MODISTools)
library(exactextractr)
options(timeout = 4000000) 

station_data = read.csv(str_interp("${getwd()}/station_data.csv"))

#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
dvortsov = data.frame(id="Dvorts",
                   latitude = c(59.568060),
                   longitude= c(30.108330))


dvortsov_around = meteo_nearby_stations(lat_lon_df = dvortsov, station_data = station_data,
                                    limit = 20, var = c("PRCP", "TAVG"),
                                    year_min = 2015, year_max = 2020)

# dvortsov_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Дворцового Парка, очевидно что первым элементом таблицы будет идентификатор метеостанции Дворцового Парка, его то мы и попытаемся получить
dvortsov_id = dvortsov_around$Dvorts$id[1]
#Для получения всех данных с метеостанции, зная ее идентификатор, используйте след. команду
all_dvortsov_data = meteo_tidy_ghcnd(stationid = dvortsov_id)

all_dvortsov_data = all_dvortsov_data %>% mutate(year = year(date)) %>% 
                                    filter(year > 2010 & year < 2022) %>% 
                                    mutate(tavg = tavg/10, prcp = prcp/10) %>% 
                                    dplyr::select(-tmax,-tmin,-snwd)

all_dvortsov_data$prcp[is.na(all_dvortsov_data$prcp)] = 0 
dvortsov_cum = all_dvortsov_data %>% mutate(month = month(date)) %>% 
                               filter(month > 4 & month < 10) %>% 
                               group_by(year) %>% 
                               mutate(prcp_cum = cumsum(prcp))

dvortsov_cum %>% summarise(prcp_avg = max(prcp_cum), n = n())

# Посмотрите для каких лет есть разумные данные и дальше работайте с одним годом, который вам больше нравится
#  1  2011     398.   153 2021     346.   153 по n все данные есть!






# Загрузите kml файл с полигоном вашего парка из любых онлайн карт
park_sf2 <- read_sf(str_interp('${getwd()}/shapes/dvortsov.geojson'))
# Сконевртируем объект в sp и загрузим для ншаей местности ЦМР из пакета elevatr
types <- vapply(sf::st_geometry(park_sf2), function(x) {
  class(x)[2]
}, "")
# В объекте park_sf2 есть два объекта, точка и мультиполигон, нам нужен полигон
park_polys <- park_sf2[ grepl("*POLYGON", types), ]
park_polys <- st_cast(park_polys, "POLYGON")

# Вырезаем его оттуда и конвертируем в spartial данные
park_sp = as_Spatial(st_zm(park_polys), 
                     cast=TRUE, 
                     IDs = paste0("ID", seq_along(from)))
prj = proj4string(park_sp)
park_dem = elevatr::get_elev_raster(park_sp, 14, prj)
plot(park_dem)
plot(st_geometry(park_polys), add = TRUE)

park_dem_mask = crop(park_dem, park_sp)
plot(park_dem_mask)
plot(st_geometry(park_polys), add = TRUE)
#qmap(park_dem_mask, park_sp)

# Самописная функция для пакета Landsat т.к. встроенная функция не работает
search_result_download = function(search_results){
  for(i in 1:nrow(search_results)){
    landsat_image_name = search_results$entit_1___2d[i]
    landsat_page_url = search_results$download_url[i]
    landsat_page = read_html(landsat_page_url, encoding = "UTF-8")
    landsat_files = landsat_page %>% html_nodes("a") %>% html_attr("href")
    landsat_path_url = landsat_page_url %>% str_remove("index.html")
    landsat_files_url = str_c(landsat_path_url, landsat_files)
    landsat_down_folder = str_c( getwd(),"/landsat/", landsat_image_name,"/")
    dir.create(landsat_down_folder)
    
    for(j in 1:length(landsat_files_url)){
      landsat_down_file = str_c(landsat_down_folder,landsat_files[j])
      print(landsat_files_url[j])
      print(landsat_down_file)
      
      download.file(landsat_files_url[j],
                    destfile = landsat_down_file,
                    method = "libcurl",
                    mode = "wb")
    }
   
  }
}


#install.packages("devtools")
library(devtools)
install_github("atlanhq/rLandsat")
 

# Найдем и скачаем данные Landsat для нашего парка
search2 = rLandsat::landsat_search(min_date = "2010-05-01", 
                                   max_date = "2021-09-30", 
                                   country = "Russian Federation", 
                                   source = "aws")
your_min_lon = summary(park_sp)[[2]][1,1]
your_max_lon = summary(park_sp)[[2]][1,2]
your_min_lat = summary(park_sp)[[2]][2,1]
your_max_lat = summary(park_sp)[[2]][2,2]
search_result = search2 %>% filter(min_lat < your_min_lat, 
                                   max_lat > your_max_lat, 
                                   max_lon > your_max_lon, 
                                   min_lon < your_min_lon, 
                                  clou_1___2over < 15)

# Долгая операция
search_result_download(search_result[6,])

# Т.к. мы скачали исходные данные с матрицы спутника, нам надо получить
# обработать метаданные с параметрами датчиков и характеристика пролета спутника
# для этого используем функции из пакета RStoolbox
ls8t = readMeta(str_interp("${getwd()}/landsat/LC81860182017136LGN00/LC08_L1TP_186018_20170516_20170525_01_T1_MTL.txt"))
lsst = stackMeta(str_interp("${getwd()}/landsat/LC81860182017136LGN00/LC08_L1TP_186018_20170516_20170525_01_T1_MTL.txt"))

ls8t_cor = radCor(lsst, 
                  metaData = ls8t, 
                  method = "apref", 
                  atmosphere = "Clear", 
                  verbose = T)
plot(ls8t_cor[[4]])
plot(ls8t_cor)

# Итак мы получили и обработали сырые данные, теперь мы имеем растровые файлы
# в которых храниться отражательная способность поверхности для ряда длинн волн
# посчитаем для имеющихся данных все вегетационные индексы, а в частности
# NDVI  = (NIR - RED)(NIR + RED), как наиболее используемый

lsst_tas = tasseledCap(ls8t_cor[[2:7]],"Landsat8OLI")
indexes = spectralIndices(ls8t_cor, blue = 2, green = 3, red = 4,
                          nir = 5, redEdge1 = NULL, redEdge2 = NULL,
                          redEdge3 = NULL, swir1 = NULL,
                           swir2 = 7, swir3 = NULL)
proj4string(indexes)
proj4string(park_sp)
park_sf_utm = st_transform(park_polys, crs = st_crs(indexes$DVI))

# Отрисуем картосхему NDVI для нашего парка
park_ndvi_crop = crop(indexes$NDVI, park_sf_utm)
plot(park_ndvi_crop)
plot(st_geometry(park_sf_utm), add = TRUE)
#Выделим все пиксели под полигоном в отдельную таблицу
ndvi_df <- exact_extract(park_ndvi_crop, park_sf_utm, include_xy=TRUE)
ndvi_df = ndvi_df[[1]]
summary(ndvi_df)
#Оставим только пиксели где была зеленая растительность
ndvi_df = ndvi_df %>% filter(value > 0.4)

#Характеристики рельефа
park_dem_utm = raster::projectRaster(from = park_dem_mask, crs = crs(park_ndvi_crop))
area_slope = terrain(park_dem_utm, opt = 'slope', unit = 'degrees')  #calculate slope
area_aspect = terrain(park_dem_utm, opt = 'aspect', unit = 'degrees') #calculate aspect
area_flowdir = terrain(park_dem_utm, opt = 'flowdir', unit = 'degrees') #calculate flowdir

plot(area_slope)
plot(st_geometry(park_sf_utm), add = TRUE)
plot(area_aspect)
plot(st_geometry(park_sf_utm), add = TRUE)
plot(area_flowdir)
plot(st_geometry(park_sf_utm), add = TRUE)


#MODISTools
#С помощью продуктов  MODIS попытаемся оценить эвапотранспирацию в парке
# Для начала посмотрим какие показатели мы можем получить от продуктов MODIS
prods = MODISTools::mt_products()
# Нам подходит MOD16A2 -  эвапотранспирация
# Посмотрим какие каналы мы можем получить по данному продукту
bands = MODISTools::mt_bands(product = "MOD16A2")
# Канал ET_500m содержит накопленные за 8 дней данные по эвапотраспирации в kg/m^2/8d
# Но мы так же должны учитывать scale factor = 0.1, что значит, что данные представлены
# в десятых долях килограммов и их нужно домножить на 0.1. Кроме того мы видим диапазон 
# допустимых значений величины, из которого следует, что значения выше 32700 надо отбросить.
# 
# Проверим для каких дат есть данные для нашей территории
dates = MODISTools::mt_dates(product = "MOD16A2", lat = 59.568060, lon = 30.108330)

# Так как данные для интересующих нас дат для изучаемой территории имеются перейдем к их получению.
# Для этого в функцию mt_subset мы должны ввести название продукта, координаты территории, канал, 
# дату начала и конца мониторинга, а также параметры km_lr и km_ab, которые будут означать в каком 
# радиусе от указаной точки будут браться пиксели с данными. У нас указано 2, что значит 2км, т.е.
# данные будут браться из окружности радиусом 4 пикселя, т.к. разрешения пикселся 500м
dvortsov_ET =  MODISTools::mt_subset(product = "MOD16A2",
                          lat = 59.568060,
                          lon =  30.108330,
                          band = "ET_500m",
                          start = "2017-05-01",
                          end = "2017-10-01",
                          km_lr = 2,
                          km_ab = 2,
                          site_name = "dvortsov",
                          internal = TRUE,
                          progress = TRUE) 

# В результате мы получили таблицу со значениями из нескольких пикселей за интересующий нас 
# промежуток времени с шагом в 8 дней. Отбросим пропуски в данных и усредним значения 
# пикселей на каждую дату, добавив переменную день года
# 
dvortsov_ET = dvortsov_ET %>% filter(value < 32700) %>% select(units,calendar_date,value) %>%
  mutate(doy=yday(calendar_date), year=year(calendar_date)) %>% group_by(doy,year,units) %>%
  summarise(ET = mean(value))

# Т.к. данные у нас идут с шагом в 8 дней, построим их сглаженное графическое
# представление с помощью loess сглаживания в ggplot2
ggplot(dvortsov_ET, aes(x=doy,y=ET))+
  geom_point()+
  geom_smooth()+
  geom_ribbon(aes(ymin = 0,ymax = predict(loess(ET ~ doy))),
              alpha = 0.3,fill = 'blue')+
  ylim(c(0,300))+
  theme_bw()



# Выглядит неплохо. Было бы здорово получить площадь под кривой, т.к. она 
# будет соответствовать усредненной сумме эвапотраспирации за вегетационный
# период. Для окончательных рассчетов нам также надо вспомнить площадь парка
# и площадь зеленых насаждений в нем 

park_area = st_area(park_sf) %>% as.integer() # площадь парка
#Вычислим площадь зеленых насаждений, т.к. в полигон у нас могли попадать пискели не целиком, для
# тех кусочков что попали частично записана их доля попавшая в полигон - coverage_fraction,
# тогда общая площадь под зелеными насаждениями будет
green_square = 0.8 * park_area[1]


# А также данные по осадкам
Prcp_cum = dvortsov_cum %>% filter(year == 2017) %>% mutate(doy = yday(date)) %>% 
  select(doy,prcp_cum) %>% mutate(water_cum = prcp_cum*park_area/1000)
start_day = min(Prcp_cum$doy)
end_day = max(Prcp_cum$doy)

# Тогда общая эвапотраспирация будет рассчитана как
curve = loess(ET ~ doy, dvortsov_ET) # модель 
ET = (predict(curve,data.frame(doy = start_day:end_day), se = F)) #0.1 * kg/m^2/8d
ET[is.na(ET)]=0
ETcum = cumsum(ET)* green_square*0.1/8/1000 #t/m2/d - вспоминаем scale factor
# делим на 8, т.к. данные это сумма за 8 дней и переводим в тонны или м3 воды

# Сводим данные по осадкам и эвапотраспирации в одну таблицу
Prcp_cum$ETcum = ETcum                        
#Посчитаем полив как разницу между накопленной с осадками влагой и 
# эвапотранспирацией, усреднив эту разницу на площадь зеленых насаждений
Prcp_cum = Prcp_cum %>% mutate(irrigation = (ETcum - water_cum)/green_square)

# Кумуляты накопленных осадков и эвапотранспирации
ggplot(Prcp_cum, aes(x = doy,y = ETcum))+
  geom_line( color="green")+
  geom_line(aes(x=doy,y=water_cum))+
  ylab("ET vs Precipitation,m3 for Dvortsov park, 2017")+
  theme_bw()
# Необходимый полив - большую часть времени полив не нужен
ggplot(Prcp_cum, aes(x = doy,y = irrigation*1000))+
  geom_line( color="red")+
  geom_hline(yintercept = 0)+
  ylab("Irrigation needed,l/m2 for Dvortsov park, 2017")+
  theme_bw()
# Оставим только ту часть, где полив нужен
ggplot(Prcp_cum, aes(x = doy,y = irrigation*1000))+
  geom_line( color="red")+
  geom_hline(yintercept = 0)+
  ylim(c(-20,100))+ # Эти параметры вам надо подобрать исходя из ваших данных
  ylab("Irrigation needed,l/m2 for Dvortsov park, 2017")+
  theme_bw()
