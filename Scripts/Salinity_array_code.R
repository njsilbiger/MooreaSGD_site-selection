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

# read in the benthic data Vararie
file_loc_benthic<-here("Data","May2021","Cond_temp","Csv_files", "QC", "Spatial_Array","051621")


files_benthic <- dir(path = file_loc_benthic,pattern = ".csv")


#GPS Vararie
gps_benthic<-read_csv(here("Data","May2021","GPS","Logger_Spatial_Array","Varari_CT_Benthic_Array_051621.csv"))


# read in the surface array data Varari
file_loc_surface<-here("Data","May2021","Cond_temp","Csv_files", "QC", "Spatial_Array","051921")

files_surface <- dir(path = file_loc_surface,pattern = ".csv")

#GPS Varari

gps_surface<-read_csv(here("Data","May2021","GPS","Logger_Spatial_Array","Varari_CT_Surface_Array_051921.csv"))

# Cabral
file_loc_cabral<-here("Data","May2021","Cond_temp","Csv_files", "QC", "Spatial_Array","052421")

files_cabral <- dir(path = file_loc_cabral,pattern = ".csv")

gps_cabral<-read_csv(here("Data","May2021","GPS","Logger_Spatial_Array","Cabral_Spatial_Array_05242021.csv"))

# Pressure for cabral
pressure_cabral <-read_csv(here("Data","May2021","Depth","Csv_files","Calibrated_csv","Pressure_870_Sled_052421.csv"), skip = 1) %>%
  select(date = `Date Time, GMT-10:00`,depth = `Water Level, meters (LGR S/N: 20810870)` ) %>%
  mutate(date = mdy_hms(date)) 

# read in the cond data #### NEED SALINITY OFF FOR BENTHIC
CondArrayData_Benthic <- files_benthic %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(~ read_csv(file.path(file_loc_benthic, .)),.id = "filename") %>%
  left_join(gps_benthic) %>% # Join the cond data with the gps data
  mutate(Surf_Benth = "Benthic") %>% # add a column for benthic
  select(date, Serial, TempInSitu, Salinity, Surf_Benth, Latitude, Longitude)

## Read in the surface data 
CondArrayData_Surface <- files_surface %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(~ read_csv(file.path(file_loc_surface, .)),.id = "filename") %>%
  left_join(gps_surface) %>% # Join the cond data with the gps data
  mutate(Surf_Benth = "Surface") %>% # add a column for surface 
  select(date, Serial, TempInSitu, Salinity = Salinity_off, Surf_Benth, Latitude, Longitude ) %>%
 mutate(Salinity = ifelse(Serial == "CT_354" & Salinity < 33, NA, Salinity ) ) %>% # remove weird spike that isnt real
  filter(Serial != "CT_345") # was exposed during low tide

# Cabral
CondArrayData_Cabral <- files_cabral %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(~ read_csv(file.path(file_loc_cabral, .)),.id = "filename") %>%
  left_join(gps_cabral) %>% # Join the cond data with the gps data
  mutate(Surf_Benth = "Cabral") %>% # add a column for surface 
  select(date, Serial, TempInSitu, Salinity = Salinity_off, Surf_Benth, Latitude, Longitude ) %>%
  right_join(pressure_cabral) %>%
  drop_na()

## Bind them together

CondArray_all <- bind_rows(CondArrayData_Benthic, CondArrayData_Surface, CondArrayData_Cabral) %>%
  mutate(time = hms(format(date, format = "%H:%M:%S")),
         sun = ifelse(time> hms('06:22:00') & time <hms('17:32:0'), "day","night"))
  

## Make a plot with benthic and surface on top of each other ####

#Surface samples
CondArray_all %>%
  filter(Surf_Benth == "Surface") %>%
  ggplot(aes(x = date, y = Salinity, color = TempInSitu, group = Serial)) +
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
CondArray_all %>%
  filter(Surf_Benth == "Benthic") %>%
   ggplot(aes(x = date, y = Salinity, color = TempInSitu, group = Serial)) +
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


#Cabral
CondArray_all %>%
  filter(Surf_Benth == "Cabral") %>%
  filter(Salinity>20) %>% # remove bad points
  ggplot(aes(x = date, y = Salinity, color = TempInSitu, group = Serial)) +
  geom_line()+
  facet_wrap(~Serial)+
  scale_color_viridis_c()+
  scale_x_datetime(date_labels = "%H %M %S")+
  labs(title = "Cabral (2021-05-24 : 2021-05-27)",
       y = "Salinity (corrected)",
       x = "",
       color = "Temperature")+
  theme_bw()+
  ggsave(here("Output","May2021","Spatial_array_plots","CabralData.png"), width = 11, height = 6)

# plot salinity versus depth

CondArray_all %>%
  filter(Surf_Benth == "Cabral") %>%
  filter(Salinity>30) %>% # remove bad points
  mutate(day = date(date)) %>%
# unite("day_sun", day,sun)%>%
  filter(day == ymd('2021-05-27') ) %>%
  ggplot(aes(x = depth, y = Salinity, color = sun)) +
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Serial, scales = 'free_y') +
  ggsave(here("Output","May2021","Spatial_array_plots","CabralData27.png"), width = 6, height = 6)

## everything just cabral on 5_24
CondArray_all %>%
  filter(Surf_Benth == "Cabral") %>%
  filter(Salinity>20) %>% # remove bad points
  mutate(day = date(date)) %>%
  # unite("day_sun", day,sun)%>%
  filter(day == ymd('2021-05-24') ) %>%
  
  ggplot(aes(x = date, y = Salinity, color = TempInSitu, group = Serial)) +
  geom_line()+
  scale_color_viridis_c()+
  scale_x_datetime(date_labels = "%H %M %S")+
  labs(title = "Cabral (2021-05-24)",
       y = "Salinity (corrected)",
       x = "",
       color = "Temperature")+
  theme_bw()+
  ggsave(here("Output","May2021","Spatial_array_plots","CabralData2.png"), width = 6, height = 6)


CondArray_all %>%
  filter(Surf_Benth == "Cabral") %>%
  filter(Salinity>20) %>% # remove bad points
  mutate(day = date(date)) %>%
  # unite("day_sun", day,sun)%>%
 # filter(day == ymd('2021-05-27') ) %>%
  
  ggplot(aes(x = date, y = depth, color = TempInSitu)) +
  geom_line()+
  ggsave(here("Output","May2021","Spatial_array_plots","Cabraldepth.png"), width = 6, height = 6)
  
### Everything on top of each other faceted by benthic and surface 
CondArray_all %>%
  filter(Salinity>20) %>% # remove bad points
  ggplot(aes(x = date, y = Salinity, color = TempInSitu, group = Serial)) +
  geom_line()+
  facet_wrap(~Surf_Benth, scales = "free_x")+
  scale_color_viridis_c()+
  scale_x_datetime(date_labels = "%H %M %S")+
  labs(y = "Salinity (corrected)",
       x = "",
       color = "Temperature")+
  theme_bw()+
  ggsave(here("Output","May2021","Spatial_array_plots","BenthicAndSurfaceData.png"), width = 9, height = 3)



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


M_coords<-data.frame(lon =	mean(CondArrayData_Surface$Longitude, na.rm = TRUE), lat = mean(CondArrayData_Surface$Latitude, na.rm = TRUE))
ArrayMap1<-get_map(M_coords, maptype = 'satellite', zoom = 18)


Cond_summary<-CondArray_all %>%
  group_by(Serial, Surf_Benth, Latitude, Longitude) %>%
  summarise(Sal_min = min(Salinity, na.rm = TRUE),
            Sal_med = median(Salinity, na.rm = TRUE),
            Sal_mean = mean(Salinity, na.rm=TRUE)) %>%
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

##Cabral
M_coords2<-data.frame(lon =	mean(CondArrayData_Cabral$Longitude, na.rm = TRUE), lat = mean(CondArrayData_Cabral$Latitude, na.rm = TRUE))
ArrayMap2<-get_map(M_coords2, maptype = 'satellite', zoom = 18)

ArrayMapmed_cabral<- ggmap(ArrayMap2)+
  geom_point(data = Cond_summary %>% filter(Surf_Benth=="Cabral"), mapping = aes(x=Longitude, y=Latitude, color = Sal_med), size = 2)+
  xlab("")+
  ylab("")+
  labs(title = "Median Salinity over 3 days",
       color = "Salinity")+
  scale_color_viridis_c() +
  geom_text_repel(data = Cond_summary %>% filter(Surf_Benth=="Cabral"), mapping = aes(x=Longitude, y=Latitude, label = Serial, color = Sal_med))+
  #facet_wrap(~Surf_Benth) +
  ggsave(here("Output","May2021","Spatial_array_plots","Map_Salinity_med_Cabral.png"), width = 10, height = 5)


##### Spatial bilge data #####
cabral_bilge_cond1<-read_csv(here("Data","May2021","Cond_temp","Csv_files","QC","Spatial_Array","052521", "QC_CT_350_052521.csv"))
cabral_bilge_cond2<-read_csv(here("Data","May2021","Cond_temp","Csv_files","QC","Spatial_Array","052521", "QC_CT_346_052521.csv"))


gps_bilge_cabral<-read_csv(here("Data","May2021","GPS","RAD_Site_Survey","Cabral_RAD_Spatial_GPS.csv"))


cabral_bilge <- bind_rows(cabral_bilge_cond1, cabral_bilge_cond2) %>%
  right_join(gps_bilge_cabral)

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
