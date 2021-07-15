### Nutrient snapshot data
### By Nyssa
##3 Created 7/13/2021



## load library#####
library(here)
library(tidyverse)
library(patchwork)
library(ggmap)
library(viridis)
library(maptools)
library(kriging)
library(ggnewscale)
library(wql)

### read in data ####
NutData<-read_csv(here("Data","May2021", "Nutrients", "Nutrient_Snap_May2021.csv"))
Locations<-read_csv(here("Data","May2021","GPS","Experimental_Locations", "Sandwich_Locations.csv"))


#### convert cond and temp to salinity

NutData <- NutData %>%
  mutate(Salinity = ec2pss(ec = Conductivity, t = Temperature, p = 0))

### make some plots####

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
#API<-names(read_table(here("Data","API.txt")))
#register_google(key = API) ### use your own API


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


### Make a spatial kriging file with polygon layers ####
### Bring in the polygons for the sites
#Varari
V_kml <- getKMLcoordinates(kmlfile=here("Data","Polygons","Varari_Polygon.kml"), ignoreAltitude=T)
#Cabral
C_kml <- getKMLcoordinates(kmlfile=here("Data","Polygons","Cabral_Polygon.kml"), ignoreAltitude=T)


# Run the kriging

VData <- AllData %>%
  filter(Location == 'Varari')

krig1 <- kriging(VData$lon, VData$lat, VData$Silicate, pixels=500,polygons=V_kml) ###pixels controls how fine or course you want the prediction data frame to be
krig2 <- krig1$map


CData <- AllData %>%
  filter(Location == 'Cabral') %>%
  select(Top_Plate_ID, lon, lat, Phosphate, Silicate, Nitrite, Ammonia)

krig3 <- kriging(CData$lon, CData$lat, CData$Silicate, pixels=500,polygons=C_kml, lags = 3) ###pixels controls how fine or course you want the prediction data frame to be
krig4 <- krig3$map



## make the map
# Varari
V_krig_map<-ggmap(ArrayMap1)+
  geom_point(data=krig2, aes(x=x, y=y, colour=pred), size=4, alpha=0.5) + 
  geom_point(data = VData, aes(x=lon, y=lat))+
  scale_color_viridis_c(name = "Silicate", option = "plasma")+
  coord_sf() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +  
  theme(panel.grid.major = element_line(color = 'white', linetype = "dashed",size = 0.5),
        plot.background=element_rect(fill='white'))+
  ggtitle("Varari")

ggsave(plot = V_krig_map, filename = here("Output","May2021","Spatial_array_plots","v_krig_map.png"))

# Cabral
C_krig_map<-ggmap(ArrayMap2)+
  geom_point(data=krig4, aes(x=x, y=y, colour=pred), size=4, alpha=0.5) + 
  geom_point(data = CData, aes(x=lon, y=lat))+
  scale_color_viridis_c(name = "Silicate", option = "plasma")+
  coord_sf() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +  
  theme(panel.grid.major = element_line(color = 'white', linetype = "dashed",size = 0.5),
        plot.background=element_rect(fill='white'))+
  ggtitle("Cabral")

ggsave(plot = C_krig_map, filename = here("Output","May2021","Spatial_array_plots","C_krig_map.png"))
