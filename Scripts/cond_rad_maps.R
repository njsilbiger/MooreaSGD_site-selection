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
library(janitor)
library(ggnewscale)
library(ggrepel)


### read in the conductivity files ###
## select all files with a .csv
CondPath<-here("Data","Cond_temp","Calibrated_files", "fixed_cal")
files <- dir(path = CondPath,pattern = ".csv")

# read in the data
CondData <- files %>%
  map(~ read_csv(file.path(CondPath, .))) %>% 
                 #col_names = c("ID", "date","HighRange", "Temp","SpC","Salinity")
  reduce(rbind) %>%
  mutate(date = ymd_hms(date)) %>% # make data a date_time
  group_by(date) %>% # there are two sensors paired. So average all data cross time
  summarise_if(is.numeric,.funs = mean) %>%
  select(date,Temp = TempInSitu, Salinity = SalinityInSitu_1pCal)
   

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
  filter(Salinity>28 & Salinity < 36.5) # remove the bad data when the sensor probably jumped out of the water

# create a coordinate set for each day to add site labels
GPS_ave<-GPS_Cond %>%
  mutate(day = day(date)) %>%
  group_by(day) %>%
  summarise(lat_mean = mean(lat),
            lon_mean = mean(lon))%>%
  mutate(SiteID = c("A", "C", "E", "B", "D", "F"))

### read in API key for maps
API<-names(read_table(here("Data","API.txt")))
register_google(key = API) ### use your own API

## All of Mo'orea ####
#M1<-get_map('Moorea',zoom = 12, maptype = 'satellite')

M_coords<-data.frame(lon =	-149.83, lat = -17.55)
M1<-get_map(M_coords, maptype = 'satellite', zoom = 12)

Mooreamap_allcond<- ggmap(M1)+
  scalebar(x.min = -149.9, x.max = -149.7,y.min = -17.63, y.max = -17.5,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = 5)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), size = 2, alpha = .60)+
  xlab("")+
  ylab("")+
  labs(color = "Salinity")+
  scale_color_gradient2(low = "yellow", high = "blue", midpoint = 31) 

### Bring them together in patchwork
# Sal_map<-(Opu1_allcond+North1_allcond)/(West2_allcond+Via1_allcond)/(West1_allcond + Mooreamap_allcond)+ plot_layout(guides = "collect")
# ggsave(here("Output", "Salinitymap.pdf"), plot = Sal_map, width = 10, height = 10)

#### Radon #####
RadPath<-here("Data","Rad","Radon_Counts")
files <- dir(path = RadPath,pattern = ".csv")

# read in the data
RadData <- files %>%
  map(~ read_csv(file.path(RadPath, .))) %>% 
  #col_names = c("ID", "date","HighRange", "Temp","SpC","Salinity")
  reduce(rbind) %>%
  clean_names() %>% # clean up the names
  mutate(date = mdy_hm(full_date)) %>% # make data a date_time
  select(date,radon = radon_in_water_dpm_l)


# The rad data is integrated over a 6 min interval while the GPS is every 10 seconds
# This join will match the rad data with the starting point of the GPS track
# I want to figure out how to fill it so it repeats the rad value for the entire 6 minute track

# the rad data lags by 12 minutes behind the actual point. So subtract 12 minutes from the tracks
GPS_alteredRad<-GPSData %>%
  mutate(date = date - minutes(12))
  
RAD_GPS<-left_join(RadData, GPS_alteredRad)

#### Make maps of each site ####

## All of Mo'orea ####

Mooreamap_allrad<- ggmap(M1)+
  scalebar(x.min = -149.9, x.max = -149.7,y.min = -17.63, y.max = -17.5,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = 5)+
  # geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), alpha = .60)+
  # scale_color_gradient(low = "yellow", high = "white") +
  # new_scale("color")+
  geom_point(data = RAD_GPS, mapping = aes(x=lon, y=lat, color = radon, size = radon))+
  geom_label_repel(data = GPS_ave, mapping = aes(x = lon_mean, y = lat_mean, label = SiteID))+
  xlab("")+
  ylab("")+
  labs(color = "Radon (DPM/L)",
       size = "Radon (DPM/L)")+
  scale_size_continuous(limits=c(0, 11), breaks=seq(0,11, by=2.5))+
  scale_color_gradient2(low = "white", high = "magenta") +
  guides(color= guide_legend(), size=guide_legend())+
  ggsave("Output/MooreaAll.pdf", width = 6, height = 6)
#  

## zoom in on each site
## West side 1
West1<-data.frame(lon =	-149.865, lat = -17.575)
M2<-get_map(West1,zoom = 16, maptype = 'satellite')

West1_allrad<- ggmap(M2)+
  scalebar(x.min = -149.871, x.max = -149.859,y.min = -17.58, y.max = -17.572,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "m", dist = 200)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), alpha = .60)+
  scale_color_gradient(low = "yellow", high = "blue") +
  new_scale("color")+
  geom_point(data = RAD_GPS, mapping = aes(x=lon, y=lat, color = radon, size = radon))+
  xlab("")+
  ylab("")+
  labs(color = "Radon (DPM/L)",
       size = "Radon (DPM/L)")+
  scale_size_continuous(limits=c(0, 11), breaks=seq(0,11, by=2.5))+
  scale_color_gradient2(low = "white", high = "magenta") +
  guides(color= guide_legend(), size=guide_legend())

## West side 2 ####

West2<-data.frame(lon =	-149.903, lat = -17.536)
M5<-get_map(West2,zoom = 16, maptype = 'satellite')

West2_allrad<- ggmap(M5)+
  scalebar(x.min = -149.905, x.max = -149.900,y.min = -17.54, y.max = -17.53,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "m", dist = 200)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), alpha = .60)+
  scale_color_gradient(low = "yellow", high = "blue") +
  new_scale("color")+
  geom_point(data = RAD_GPS, mapping = aes(x=lon, y=lat, color = radon, size = radon))+
  xlab("")+
  ylab("")+
  labs(color = "Radon (DPM/L)",
       size = "Radon (DPM/L)")+
  scale_size_continuous(limits=c(0, 11), breaks=seq(0,11, by=2.5))+
  scale_color_gradient2(low = "white", high = "magenta") +
  guides(color= guide_legend(), size=guide_legend())


## North Shore ####
North1<-data.frame(lon =	-149.794, lat = -17.47934142)
M3<-get_map(North1,zoom = 18, maptype = 'satellite')

North1_allrad<- ggmap(M3)+
  scalebar(x.min = -149.795, x.max = -149.793,y.min = -17.4805, y.max = -17.478,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "topleft", transform = TRUE, dist_unit = "m", dist = 50)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), alpha = .60)+
  scale_color_gradient(low = "yellow", high = "blue") +
  new_scale("color")+
  geom_point(data = RAD_GPS, mapping = aes(x=lon, y=lat, color = radon, size = radon))+
  xlab("")+
  ylab("")+
  labs(color = "Radon (DPM/L)",
       size = "Radon (DPM/L)")+
  scale_size_continuous(limits=c(0, 11), breaks=seq(0,11, by=2.5))+
  scale_color_gradient2(low = "white", high = "magenta") +
  guides(color= guide_legend(), size=guide_legend())


## Opunahu
Opu1<-data.frame(lon =	-149.869, lat = -17.4932)
M4<-get_map(Opu1,zoom = 18, maptype = 'satellite')

Opu1_allrad<- ggmap(M4)+
  scalebar(x.min = -149.87, x.max = -149.860,y.min = -17.4945, y.max = -17.492,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "m", dist = 50)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), alpha = .60)+
  scale_color_gradient(low = "yellow", high = "blue") +
  new_scale("color")+
  geom_point(data = RAD_GPS, mapping = aes(x=lon, y=lat, color = radon, size = radon))+
  xlab("")+
  ylab("")+
  labs(color = "Radon (DPM/L)",
       size = "Radon (DPM/L)")+
  scale_size_continuous(limits=c(0, 11), breaks=seq(0,11, by=2.5))+
  scale_color_gradient2(low = "white", high = "magenta") +
  guides(color= guide_legend(), size=guide_legend())


## East side
# Viarae
Via1<-data.frame(lon =	-149.773, lat = -17.5295)
M6<-get_map(Via1,zoom = 17, maptype = 'satellite')

Via1_allrad<- ggmap(M6)+
  scalebar(x.min = -149.776, x.max = -149.770,y.min = -17.532, y.max = -17.528,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "m", dist = 100)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), alpha = .60)+
  scale_color_gradient(low = "yellow", high = "blue") +
  new_scale("color")+
  geom_point(data = RAD_GPS, mapping = aes(x=lon, y=lat, color = radon, size = radon))+
  xlab("")+
  ylab("")+
  labs(color = "Radon (DPM/L)",
       size = "Radon (DPM/L)")+
  scale_size_continuous(limits=c(0, 11), breaks=seq(0,11, by=2.5))+
  scale_color_gradient2(low = "white", high = "magenta") +
  guides(color= guide_legend(), size=guide_legend())


## East 2
	
East2<-data.frame(lon =	-149.790, lat = -17.554)
M7<-get_map(East2,zoom = 17, maptype = 'satellite')

East2_allrad<- ggmap(M7)+
  scalebar(x.min = -149.792, x.max = -149.788,y.min = -17.556, y.max = -17.552,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "m", dist = 100)+
  geom_point(data = GPS_Cond, mapping = aes(x=lon, y=lat, color = Salinity), alpha = .60)+
  scale_color_gradient(low = "yellow", high = "blue") +
  new_scale("color")+
  geom_point(data = RAD_GPS, mapping = aes(x=lon, y=lat, color = radon, size = radon))+
  xlab("")+
  ylab("")+
  labs(color = "Radon (DPM/L)",
       size = "Radon (DPM/L)")+
  scale_size_continuous(limits=c(0, 11), breaks=seq(0,11, by=2.5))+
  scale_color_gradient2(low = "white", high = "magenta") +
  guides(color= guide_legend(), size=guide_legend())


### Bring them together in patchwork
Rad_map<-(Opu1_allrad+North1_allrad)/(West2_allrad+Via1_allrad)/(West1_allrad + East2_allrad)+
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")
ggsave(here("Output", "Radmap.pdf"), plot = Rad_map, width = 10, height = 10)

Mooreamap_allrad + Mooreamap_allcond+
  plot_layout(guides = "collect") 
