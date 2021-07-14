### Nutrient snapshot data
### By Nyssa
##3 Created 7/13/2021



## load library#####
library(here)
library(tidyverse)
library(patchwork)
library(ggmap)
library(viridis)

### read in data ####
NutData<-read_csv(here("Data","May2021", "Nutrients", "Nutrient_Snap_May2021.csv"))
Locations<-read_csv(here("Data","May2021","GPS","Experimental_Locations", "Sandwich_Locations.csv"))


p1<-NutData %>%
  right_join(Locations) %>%
  ggplot(aes(x = Phosphate, y = Nitrite)) +
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~Location, scales = "free")+
  theme_bw()

p2<-NutData %>%
  right_join(Locations) %>%
  ggplot(aes(x = Ammonia, y = Nitrite)) +
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~Location, scales = "free")+
  theme_bw()

p3<-NutData %>%
  right_join(Locations) %>%
  ggplot(aes(x = Silicate, y = Nitrite)) +
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~Location, scales = "free")+
  theme_bw()


NutData %>%
  right_join(Locations) %>%
  filter(Phosphate<0.6) %>%
    ggplot(aes(x = Phosphate, y = Nitrite)) +
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~Location, scales = "free")+
  theme_bw()

p4<-NutData %>%
  right_join(Locations) %>%
  ggplot(aes(x = Phosphate, y = Ammonia)) +
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~Location, scales = "free")+
  theme_bw()

p1/p2/p3

##### Make Maps #########
API<-names(read_table(here("Data","API.txt")))
register_google(key = API) ### use your own API


# mean lat and long for the maps

MeanGPS<-Locations %>%
  group_by(Location) %>%
  summarise(lon = mean(lon),
            lat = mean(lat))

# combined data
AllData <- NutData %>%
  right_join(Locations) 

## Vaiari

ArrayMap1<-get_map(MeanGPS %>% filter(Location == "Varari") %>% select(lon,lat), maptype = 'satellite', zoom = 18)


Map_V1<-ggmap(ArrayMap1) +
  geom_point(data = AllData %>% filter(Location =="Varari"), aes(x = lon, y = lat, color = Nitrite), size = 4)+
  scale_color_viridis_c(trans = "sqrt", option = "plasma", direction = -1)+
  ggtitle("Varari")

Map_V2<-ggmap(ArrayMap1) +
  geom_point(data = AllData %>% filter(Location =="Varari"), aes(x = lon, y = lat, color = Silicate), size = 4)+
  scale_color_viridis_c(trans = "sqrt", option = "plasma", direction = -1)+
  ggtitle("Varari")




# Cabral
ArrayMap2<-get_map(MeanGPS %>% filter(Location == "Cabral") %>% select(lon,lat), maptype = 'satellite', zoom = 18)

Map_C1<-ggmap(ArrayMap2)+
  geom_point(data = AllData %>% filter(Location =="Cabral"), aes(x = lon, y = lat, color = Nitrite), size = 5)+
  scale_color_viridis_c(option = "plasma", trans = "sqrt", direction = -1)+
  ggtitle("Cabral (15-19 uM at seep)")


Map_C2<-ggmap(ArrayMap2)+
  geom_point(data = AllData %>% filter(Location =="Cabral"), aes(x = lon, y = lat, color = Silicate), size = 5)+
  scale_color_viridis_c(option = "plasma", trans = "sqrt", direction = -1)+
  ggtitle("Cabral (370-431 uM at seep)")


(Map_V1 + Map_V2)/(Map_C1 + Map_C2) 
