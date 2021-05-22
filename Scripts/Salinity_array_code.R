### Map of salinity arrays

## load libraries ####
library(tidyverse)
library(gganimate)
library(ggmap)
library(here)
library(lubridate)

### Read in the data ####

file_loc<-here("Output","May2021","QC", "Spatial","051621")

files <- dir(path = file_loc,pattern = ".csv")


# Read in the gps data

gps<-read_csv(here("Data","May2021","GPS","SpatialArray_0518.csv"))

# read in the cond data
CondArrayData <- files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(~ read_csv(file.path(file_loc, .)),.id = "filename") %>%
  left_join(gps) %>%# Join the cond data with the gps data
  filter(Serial != "CT_316") # something weird here


# get hourly averages
CondArrayData_hourly<- CondArrayData %>%
  group_by(date = floor_date(date, "hour"), Serial) %>%
  summarise(Sal_hour = mean(Salinity, na.rm = TRUE),
            lat = mean(lat, na.rm=TRUE),
            long = mean(long, na.rm=TRUE))

### read in API key for maps
API<-names(read_table(here("Data","API.txt")))
register_google(key = API) ### use your own API


M_coords<-data.frame(lon =	mean(CondArrayData$long, na.rm = TRUE), lat = mean(CondArrayData$lat, na.rm = TRUE))
ArrayMap1<-get_map(M_coords, maptype = 'satellite', zoom = 19)

ArrayMap<- ggmap(ArrayMap1)+
  geom_point(data = CondArrayData_hourly, mapping = aes(x=long, y=lat, color = Sal_hour), size = 2, alpha = .60)+
  xlab("")+
  ylab("")+
  scale_color_gradient2(low = "red",
                        mid = "white",
                        high = "blue",
                        midpoint = 32)

# make an animation over time
ArrayMap +
transition_time(date) +
  labs(title = "Time: {frame_time}")
