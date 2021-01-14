### script to map salinity and radon at sites across Mo'orea###
### Created 1/13/2021 ###
### Updated 1/13/2021 ###
### Created by Nyssa Silbiger###
#################################

## Load the libraries ####
library(tidyverse)
library(ggmap)
library(here)
library(lubridate)
library(sf)
library(ggsn)
library(ggrepel)
library(geodist)
library(calecopal)
library(patchwork)


### read in the conductivity files ###
## select all files with a .csv
CondPath<-here("Data","Cond_temp","Calibrated_files")
files <- dir(path = CondPath,pattern = "*hobo.csv")

# read in the data
CondData <- files %>%
  map(~ read_csv(file.path(CondPath, .), skip = 2, col_names = c("ID", "date","HighRange", "Temp","SpC","Salinity"))) %>% 
  reduce(rbind) %>%
  mutate(date = mdy_hms(date)) %>% # make data a date_time
  group_by(date) %>% # there are two sensors paired. So average all data cross time
  summarise_if(is.numeric,.funs = mean) %>%
  select(date,Temp, SpC, Salinity)
   

#### Read in GPS data ####
GPSPath<-here("Data","GPS")
files <- dir(path = GPSPath,pattern = "*.csv")

# read in the data
GPSData <- files %>%
  map(~ read_csv(file.path(GPSPath, .), skip = 42)) %>% 
  reduce(rbind) %>%
  mutate(date = ymd_hms(time), # make data a date_time
         date = date - hours(10), # GPS was off by 10 hours
         date = round_date(date, "10 seconds")) %>% # round to nearest 10 seconds to match with cond
  # it is also off by hours
  select(date, lat, lon) %>%# only pull what we need
  arrange(-desc(date))

# join the GPS and conductivity data ####
GPS_Cond<-left_join(GPSData, CondData) %>%
  drop_na(Salinity) %>% # drop missing data
  filter(Salinity>28) # remove the bad data when the sensor probably jumped out of the water

### read in API key for maps
API<-names(read_table(here("Data","API.txt")))
register_google(key = API) ### use your own API

## All of Mo'orea ####
M1<-get_map('Moorea',zoom = 12, maptype = 'satellite')

Mooreamap_allcond<- ggmap(M1)+
  scalebar(x.min = -149.9, x.max = -149.7,y.min = -17.63, y.max = -17.5,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = 5)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), size = 2, alpha = .60)+
  xlab("")+
  ylab("")+
  labs(color = "Salinity")+
  scale_color_gradient2(low = "lightgreen", high = "red", midpoint = 31) 

## zoom in on each site
## West side 1
West1<-data.frame(lon =	-149.865, lat = -17.575)

M2<-get_map(West1,zoom = 16, maptype = 'satellite')
West1_allcond<- ggmap(M2)+
  # scalebar(x.min = -149.9, x.max = -149.7,y.min = -17.63, y.max = -17.5,
  #          model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
  #          location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = 5)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), size = 2, alpha = .60)+
  xlab("")+
  ylab("")+
  labs(color = "Salinity")+
  scale_color_gradient2(low = "lightgreen", high = "red", midpoint = 31) 

## West side 2 ####

	
West2<-data.frame(lon =	-149.903, lat = -17.536)
M5<-get_map(West2,zoom = 16, maptype = 'satellite')
West2_allcond<- ggmap(M5)+
  # scalebar(x.min = -149.9, x.max = -149.7,y.min = -17.63, y.max = -17.5,
  #          model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
  #          location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = 5)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), size = 2, alpha = .60)+
  xlab("")+
  ylab("")+
  labs(color = "Salinity")+
  scale_color_gradient2(low = "lightgreen", high = "red", midpoint = 31)


## North Shore ####

North1<-data.frame(lon =	-149.794, lat = -17.47934142)
M3<-get_map(North1,zoom = 18, maptype = 'satellite')
North1_allcond<- ggmap(M3)+
  # scalebar(x.min = -149.9, x.max = -149.7,y.min = -17.63, y.max = -17.5,
  #          model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
  #          location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = 5)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), size = 2, alpha = .60)+
  xlab("")+
  ylab("")+
  labs(color = "Salinity")+
  scale_color_gradient2(low = "lightgreen", high = "red", midpoint = 31) 

## Opunahu
Opu1<-data.frame(lon =	-149.8700656, lat = -17.49)
M4<-get_map(Opu1,zoom = 14, maptype = 'satellite')
Opu1_allcond<- ggmap(M4)+
  # scalebar(x.min = -149.9, x.max = -149.7,y.min = -17.63, y.max = -17.5,
  #          model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
  #          location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = 5)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), size = 2, alpha = .60)+
  xlab("")+
  ylab("")+
  labs(color = "Salinity")+
  scale_color_gradient2(low = "lightgreen", high = "red", midpoint = 31) 


### Bring them together in patchwork
(Opu1_allcond+North1_allcond)/(West1_allcond+West2_allcond)+ plot_layout(guides = "collect")
