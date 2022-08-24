##GFW Data Exploration

#load the packages you need for the loop below
library(tidyverse)
library(raster)
library(ggpubr)
# library(lubridate)
# library(data.table)

#loop through all files to combine them into one dataframe
#When you download from GFW, it will be a folder with a name like "mmsi-daily-csvs-10-v2-2020"
#I've already done this for you so you don't need to run it again (it takes a while)

# files <- list.files(path="mmsi-daily-csvs-10-v2-2020", pattern="*.csv", full.names=TRUE, recursive=TRUE)
# datalist = list()

#remove all nonfishing vessels in all csv files in that folder
# for (i in 1:length(files)){
 # temp=read.csv(files[i])
 # temp$date <- as.Date(temp$date)
 # temp$month <- months(temp$date)
 # temp$year <- year(temp$date)
 # temp$i <- i
 # temp <- subset(temp, fishing_hours > 0)
 # datalist[[i]] <- temp
# }

# fishing_2020 = do.call(rbind, datalist)
# write.csv(fishing_2020, "fishing_2020.csv")

##now you have a new file containing all the fishing in 2020
#read in the csv since you didn't run the loop above - it's a large file so this will take a couple minutes
fishing_2020 <- read.csv("fishing_2020.csv")

#Rough boundary around Hawai'i
fishing_2020_hi <- fishing_2020 %>%
  filter(cell_ll_lat > 18 & cell_ll_lat < 23 & cell_ll_lon < -154 & cell_ll_lon > -160) %>%
  group_by(date, month, year, cell_ll_lat, cell_ll_lon) %>%
  summarize(sum_fishing_hours = sum(fishing_hours))

fishing_2020_hi$month <- factor(fishing_2020_hi$month, levels=c("January", "February", "March", "April", "May", "June", "July", "August",
                                                             "September", "October", "November", "December"))

ggplot(data=fishing_2020_hi %>% 
         group_by(month) %>% summarize(monthly_total = sum(sum_fishing_hours)),
                                       aes(x=month, y=monthly_total)) + 
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


HI_raster <- rasterFromXYZ(fishing_2020_hi[, c('cell_ll_lon', 'cell_ll_lat', 'sum_fishing_hours')])
spplot(HI_raster)


#Same idea but a more global level
#We're going to filter it just to look at one month so it's a little easier to work with
fishing_2020_jan <- fishing_2020 %>%
  filter(month == "January") %>%
  group_by(date, month, year, cell_ll_lat, cell_ll_lon) %>%
  summarize(sum_fishing_hours = sum(fishing_hours))


p1 <- ggplot(data=fishing_2020_jan %>% 
         group_by(date) %>% summarize(total_fishing = sum(sum_fishing_hours)),
       aes(x=date, y=total_fishing)) + 
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

jan_raster <- rasterFromXYZ(fishing_2020_jan[, c('cell_ll_lon', 'cell_ll_lat', 'sum_fishing_hours')])
spplot(jan_raster)

#same for august
fishing_2020_aug <- fishing_2020 %>%
  filter(month == "August") %>%
  group_by(date, month, year, cell_ll_lat, cell_ll_lon) %>%
  summarize(sum_fishing_hours = sum(fishing_hours))


p2 <- ggplot(data=fishing_2020_aug %>% 
         group_by(date) %>% summarize(total_fishing = sum(sum_fishing_hours)),
       aes(x=date, y=total_fishing)) + 
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

aug_raster <- rasterFromXYZ(fishing_2020_aug[, c('cell_ll_lon', 'cell_ll_lat', 'sum_fishing_hours')])
spplot(aug_raster)

ggarrange(p1, p2, nrow=2)







######Using the new GFW R package
#devtools::install_github("GlobalFishingWatch/gfwr")
library(gfwr)

#instructions on how to get your own API token:
#https://github.com/GlobalFishingWatch/gfwr

#we can use mine for now
key <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJBbm5pZSBJbm5lcy1Hb2xkIiwidXNlcklkIjoxMTc4MSwiYXBwbGljYXRpb25OYW1lIjoiQW5uaWUgSW5uZXMtR29sZCIsImlkIjo2MiwidHlwZSI6InVzZXItYXBwbGljYXRpb24ifSwiaWF0IjoxNjU4NDI4MDY3LCJleHAiOjE5NzM3ODgwNjcsImF1ZCI6ImdmdyIsImlzcyI6ImdmdyJ9.bCzCsBeZHk2eJQN8e3bz2EvUgFzgC_-1Hm0DcTGMB20C8rlpYNlAcVVKm4D5cqxfVlzy3pvuK0Fcj9Wf7-rwpt1Y8PMmhtlDzoR-bJmkfVjFfsiUCZt0B-NqyDt6gvjXBHqiobz83vxKDWVU3-FUAP8W9FKErmrkMqcRaclj8wLuMSEpklYtZlPXOi9ExSShukwKz60ICOYUCGejvuiM0MZYGhhk2Jr9a22Ja2FwTuUmMmzuDEm1PAHGL14zD-iH0AMEBFBOQnLSfiMPTC5AtrS-NISBhTw6P0VEL8MVcWGTGBjZReTVajS7-Q5zGa-6NW9u3QtlgB11AhNnp5JYWXUjbCas7fLq5Almqnt53Lp08n2ZpGGT4HjWHS0TrTBEY4KRBv-ogTVBi278AATtkQbkRVCoyHPiKs-htmZ02YgCcyqLrCdV0OBbr2wLps9hVnhCitvs3nuWs3KPG1SO1dQayYvFqHPxsMSY2olPXiJjWPxrOQYluWwxF7a71Xwy"

###get_vessel_info###
#search by mmsi
get_vessel_info(query = 224224000, search_type = "basic", 
                dataset = "all", key = key)
#search by shipname
get_vessel_info(query = "shipname LIKE '%GABU REEFE%' OR imo = '8300949'", 
                search_type = "advanced", dataset = "carrier_vessel", key = key)

#search by vessel ID
get_vessel_info(query = "8c7304226-6c71-edbe-0b63-c246734b3c01", 
                search_type = "id", dataset = "carrier_vessel", key = key)


###get_event###
#Let’s say that you don’t know the vessel id but you have the MMSI (or other identity information).
#You can use get_vessel_info function first to extract vessel id and then use it in the get_event function:
vessel_id <- get_vessel_info(query = 224224000, search_type = "basic", key = key)$id

get_event(event_type='port_visit',
          vessel = vessel_id,
          confidences = '4',
          key = key)

#get encounters for all vessels in a given date range
get_event(event_type='encounter',
          start_date = "2020-01-01",
          end_date = "2020-02-01",
          key = key
)

#download list of USA trawlers
usa_fishing <- get_vessel_info(
  query = "flag = 'USA'", 
  search_type = "advanced", 
  dataset = "fishing_vessel",
  key=key
)

# Collapse vessel ids into a commas separated list to pass to Events API
usa_fishing_ids <- paste0(usa_fishing$id[1:10], collapse = ',')

#Now get the list of fishing events for these trawlers in January, 2020:
get_event(event_type='fishing',
          vessel = usa_fishing_ids,
          start_date = "2019-01-01",
          end_date = "2020-02-01",
          key=key)
??get_event


###Making a raster###

#Here’s an example where we enter the geojson data manually:
region_json = '{"geojson":{"type":"Polygon","coordinates":[[[-76.11328125,-26.273714024406416],[-76.201171875,-26.980828590472093],[-76.376953125,-27.527758206861883],[-76.81640625,-28.30438068296276],[-77.255859375,-28.767659105691244],[-77.87109375,-29.152161283318918],[-78.486328125,-29.45873118535532],[-79.189453125,-29.61167011519739],[-79.892578125,-29.6880527498568],[-80.595703125,-29.61167011519739],[-81.5625,-29.382175075145277],[-82.177734375,-29.07537517955835],[-82.705078125,-28.6905876542507],[-83.232421875,-28.071980301779845],[-83.49609375,-27.683528083787756],[-83.759765625,-26.980828590472093],[-83.84765625,-26.35249785815401],[-83.759765625,-25.64152637306576],[-83.583984375,-25.16517336866393],[-83.232421875,-24.447149589730827],[-82.705078125,-23.966175871265037],[-82.177734375,-23.483400654325635],[-81.5625,-23.241346102386117],[-80.859375,-22.998851594142906],[-80.15625,-22.917922936146027],[-79.453125,-22.998851594142906],[-78.662109375,-23.1605633090483],[-78.134765625,-23.40276490540795],[-77.431640625,-23.885837699861995],[-76.9921875,-24.28702686537642],[-76.552734375,-24.846565348219727],[-76.2890625,-25.48295117535531],[-76.11328125,-26.273714024406416]]]}}'

get_raster(spatial_resolution = 'low',
           temporal_resolution = 'yearly',
           group_by = 'flag',
           date_range = '2020-01-01,2021-10-01',
           region = region_json,
           region_source = 'user_json',
           key = key)

# use EEZ function to get EEZ code of Cote d'Ivoire
code_eez <- get_region_id(region_name = 'CIV', region_source = 'eez', key = key)

get_raster(spatial_resolution = 'low',
           temporal_resolution = 'yearly',
           group_by = 'flag',
           date_range = '2021-01-01,2021-10-01',
           region = code_eez$id,
           region_source = 'eez',
           key = key)

#You could search for just one word in the name of the EEZ and then decide which one you want:
(get_region_id(region_name = 'French', region_source = 'eez', key = key))

# Let's say we're interested in the French Exclusive Economic Zone, 5677
ras <- get_raster(spatial_resolution = 'low',
                  temporal_resolution = 'yearly',
                  group_by = 'flag',
                  date_range = '2021-01-01,2021-10-01',
                  region = 8456, #France 5677
                  region_source = 'eez',
                  key = key)
dfr <- rasterFromXYZ(ras)  #Convert first two columns as lon-lat and third as value                
spplot(dfr$Apparent.Fishing.hours)



# use region id function to get MPA code of Phoenix Island Protected Area
code_mpa <- get_region_id(region_name = 'Phoenix', region_source = 'mpa', key = key)

ras <- get_raster(spatial_resolution = 'low',
           temporal_resolution = 'yearly',
           group_by = 'flag',
           date_range = '2015-01-01,2018-01-01',
           region = code_mpa$id[1],
           region_source = 'mpa',
           key = key)

dfr <- rasterFromXYZ(ras)  #Convert first two columns as lon-lat and third as value                
spplot(dfr$Apparent.Fishing.hours)

#It is also possible to filter rasters to one of the five regional fisheries management organizations (RFMO) that manage tuna and tuna-like species. 
#These include "ICCAT", "IATTC","IOTC", "CCSBT" and "WCPFC".
ras <- get_raster(spatial_resolution = 'low',
           temporal_resolution = 'daily',
           group_by = 'flag',
           date_range = '2021-01-01,2021-01-15',
           region = 'ICCAT',
           region_source = 'trfmo',
           key = key)

dfr <- rasterFromXYZ(ras)  #Convert first two columns as lon-lat and third as value                
spplot(dfr$Apparent.Fishing.hours)

