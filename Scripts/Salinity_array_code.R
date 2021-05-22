### Map of salinity arrays in the benthos and on the surface.

## load libraries ####
library(tidyverse)
library(gganimate)
library(ggmap)
library(here)
library(lubridate)
library(viridis)
library(ggrepel)

### Read in the data ####

# read in the benthic data
file_loc_benthic<-here("Output","May2021","QC", "Spatial","051621")


files_benthic <- dir(path = file_loc_benthic,pattern = ".csv")

#GPS
gps_benthic<-read_csv(here("Data","May2021","GPS","Varari_CT_Benthic_Array_051621.csv"))


# read in the surface array data
file_loc_surface<-here("Output","May2021","QC", "Spatial","051921")

files_surface <- dir(path = file_loc_surface,pattern = ".csv")

#GPS

gps_surface<-read_csv(here("Data","May2021","GPS","Varari_CT_Surface_Array_051921.csv"))


# read in the cond data ####
CondArrayData_Benthic <- files_benthic %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(~ read_csv(file.path(file_loc_benthic, .)),.id = "filename") %>%
  left_join(gps_benthic) %>% # Join the cond data with the gps data
  mutate(Surf_Benth = "Benthic") %>% # add a column for benthic
  select(date, Serial, TempInSitu, Salinity_off, Surf_Benth, Latitude, Longitude)

## Read in the surface data 
CondArrayData_Surface <- files_surface %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(~ read_csv(file.path(file_loc_surface, .)),.id = "filename") %>%
  left_join(gps_surface) %>% # Join the cond data with the gps data
  mutate(Surf_Benth = "Surface") %>% # add a column for surface 
  select(date, Serial, TempInSitu, Salinity_off, Surf_Benth, Latitude, Longitude ) %>%
 mutate(Salinity_off = ifelse(Serial == "CT_354" & Salinity_off < 33, NA, Salinity_off ) ) %>% # remove weird spike that isnt real
  filter(Serial != "CT_345") # was exposed during low tide


## Bind them together

CondArray_all <- bind_rows(CondArrayData_Benthic, CondArrayData_Surface) %>%
  mutate(time = format(date, format = "%H:%M:%S"))
  

## Make a plot with benthic and surface on top of each other ####

#Surface samples
CondArray_all %>%
  filter(Surf_Benth == "Surface") %>%
  ggplot(aes(x = date, y = Salinity_off, color = TempInSitu, group = Serial)) +
  geom_line()+
  facet_wrap(~Serial)+
  scale_color_viridis_c()+
  scale_x_datetime(date_labels = "%H %M %S")+
  labs(title = "Surface (2021-05-19 : 2021-05-21)",
       y = "Salinity (corrected)",
       x = "",
       color = "Temperature")+
  theme_bw()+
  ggsave(here("Output","May2021","Spatial_array_plots","SurfaceData.png"), width = 11, height = 6)

## Benthic figure
#Surface samples
CondArray_all %>%
  filter(Surf_Benth == "Benthic") %>%
   ggplot(aes(x = date, y = Salinity_off, color = TempInSitu, group = Serial)) +
  geom_line()+
  facet_wrap(~Serial)+
  scale_color_viridis_c()+
  scale_x_datetime(date_labels = "%H %M %S")+
  labs(title = "Benthic (2021-05-16 : 2021-05-18)",
       y = "Salinity (corrected)",
       x = "",
       color = "Temperature")+
  theme_bw()+
  ggsave(here("Output","May2021","Spatial_array_plots","BenthicData.png"), width = 11, height = 6)


### Everything on top of each other faceted by benthic and surface 
CondArray_all %>%
  ggplot(aes(x = date, y = Salinity_off, color = TempInSitu, group = Serial)) +
  geom_line()+
  facet_wrap(~Surf_Benth, scales = "free_x")+
  scale_color_viridis_c()+
  scale_x_datetime(date_labels = "%H %M %S")+
  labs(y = "Salinity (corrected)",
       x = "",
       color = "Temperature")+
  theme_bw()+
  ggsave(here("Output","May2021","Spatial_array_plots","BenthicAndSurfaceData.png"), width = 6, height = 3)

# # get hourly averages
# CondArrayData_hourly<- CondArrayData %>%
#   group_by(date = floor_date(date, "hour"), Serial) %>%
#   summarise(Sal_hour = mean(Salinity, na.rm = TRUE),
#             Temp_hour = mean(TempInSitu, na.rm = TRUE),
#             lat = mean(Latitude, na.rm=TRUE),
#             long = mean(Longitude, na.rm=TRUE))


## Make maps that highlight the minimum values in the benthic and surface samples ####

### read in API key for maps
API<-names(read_table(here("Data","API.txt")))
register_google(key = API) ### use your own API


M_coords<-data.frame(lon =	mean(CondArrayData_Benthic$Longitude, na.rm = TRUE), lat = mean(CondArrayData_Benthic$Latitude, na.rm = TRUE))
ArrayMap1<-get_map(M_coords, maptype = 'satellite', zoom = 18)


Cond_summary<-CondArray_all %>%
  group_by(Serial, Surf_Benth, Latitude, Longitude) %>%
  summarise(Sal_min = min(Salinity_off, na.rm = TRUE),
            Sal_med = median(Salinity_off, na.rm = TRUE),
            Sal_mean = mean(Salinity_off, na.rm=TRUE)) %>%
  drop_na()


ArrayMapmin<- ggmap(ArrayMap1)+
  geom_point(data = Cond_summary, mapping = aes(x=Longitude, y=Latitude, color = Sal_min), size = 2)+
  # geom_label_repel(data = Cond_summary, mapping = aes(x=Longitude, y=Latitude,label = Serial))+
  xlab("")+
  ylab("")+
  labs(title = "Min Salinity over 2 days",
       color = "Salinity")+
  scale_color_viridis_c() +
   facet_wrap(~Surf_Benth) +
  ggsave(here("Output","May2021","Spatial_array_plots","Map_Salinity_min.png"), width = 7, height = 3)


ArrayMapmed<- ggmap(ArrayMap1)+
  geom_point(data = Cond_summary, mapping = aes(x=Longitude, y=Latitude, color = Sal_med), size = 2)+
  xlab("")+
  ylab("")+
  labs(title = "Median Salinity over 2 days",
       color = "Salinity")+
  scale_color_viridis_c() +
  facet_wrap(~Surf_Benth) +
  ggsave(here("Output","May2021","Spatial_array_plots","Map_Salinity_med.png"), width = 7, height = 3)


# ArrayMap<- ggmap(ArrayMap1)+
#   geom_point(data = CondArrayData_hourly, mapping = aes(x=long, y=lat, color = Sal_hour), size = 2, alpha = .60)+
#   xlab("")+
#   ylab("")+
#   scale_color_gradient2(low = "red",
#                         mid = "white",
#                         high = "blue",
#                         midpoint = 32)

# make an animation over time
# ArrayMap +
# transition_time(date) +
#   labs(title = "Time: {frame_time}")


# Flat map of the min values

# CondArrayData_Benthic %>%
#   ggplot(aes(x = date, y = Salinity, color = TempInSitu, group = Serial)) +
#   geom_line() +
#   facet_wrap(~Serial)+
#   ggsave(here("Output", "May2021", "SalArrayTime.png"), width = 7, height = 6)
# 
# CondArrayData_hourly %>%
#   ggplot(aes(x = date, y = Temp_hour, color = lat, group = Serial)) +
#   geom_line() +
#   facet_wrap(~Serial)+
#   ggsave(here("Output", "May2021", "TempArrayTime_hour.png"))
# 
# CondArrayData_hourly %>%
#   ggplot(aes(x = date, y = Temp_hour, color = lat, group = Serial)) +
#   geom_line() +
#  # facet_wrap(~Serial)+
#   ggsave(here("Output", "May2021", "TempArrayTime_hourall.png"))
