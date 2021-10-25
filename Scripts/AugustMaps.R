### Maps of all parameters over time ####
### Nyssa Silbiger ####
### 10/22/2021 ####

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


## Read in data

AllChemData<-read_csv(here("Data","August2021","Allbiogeochemdata_QC.csv"))

# mean lat and long for the maps
MeanGPS<-AllChemData %>%
  filter(Location != "Offshore")%>%
  group_by(Location) %>%
  summarise(lon = mean(lon, na.rm = TRUE),
            lat = mean(lat, na.rm = TRUE))

# Base Maps
#API<-names(read_table(here("Data","API.txt")))
#register_google(key = API) ### use your own API

VarariBaseMap<-get_map(MeanGPS %>% filter(Location == "Varari") %>% select(lon,lat), maptype = 'satellite', zoom = 18)

# Cabral
CabralBaseMap<-get_map(MeanGPS %>% filter(Location == "Cabral") %>% select(lon,lat), maptype = 'satellite', zoom = 18)

### Make a spatial kriging file with polygon layers ####
### Bring in the polygons for the sites
#Varari
V_kml <- getKMLcoordinates(kmlfile=here("Data","Polygons","Varari_Polygon.kml"), ignoreAltitude=T)
#Cabral
C_kml <- getKMLcoordinates(kmlfile=here("Data","Polygons","Cabral_Polygon.kml"), ignoreAltitude=T)



# Get the data ranges for all variables to cleaner plots
DataRange <-AllChemData %>%
  filter(Plate_Seep == "Plate") %>% 
  select(-Temperature)%>%
  group_by(Location) %>% 
  summarise_if(is.numeric, range, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(min_max = c("min","max","min","max")) %>%
  pivot_longer(cols = Salinity:Ammonia_umolL, names_to = "Parameters", values_to = "Values") %>%
  pivot_wider(names_from = min_max, values_from = Values) %>%
  select(Location, Parameters, min, max)

mins<-DataRange %>%
  filter(is.na(max)) %>%
  select(-max)

maxs<-DataRange %>%
  filter(is.na(min)) %>%
  select(-min)

min_max<-left_join(mins,maxs)

# make a function to do all the krigings
Krig_function <-function(dat_in = data, Lat = "lat", Lon = "lon", Param = "Values", poly ) {
  
  dat <- dat_in[,c(Lon, Lat, Param)]
  names(dat) <- c('Lon', 'Lat', 'Param') 
# VData <- AllChemData %>%
#   filter(Location == location,
#          Tide == tide,
#          Day_Night == day_night,
#          Date == date,
#          Plate_Seep == "Plate") %>%
#   drop_na({{parameter}})

x <- dat$Lon
y <- dat$Lat
z <-dat$Param

krig1 <- kriging(x, y, z, pixels=500,polygons=poly) ###pixels controls how fine or course you want the prediction data frame to be
krig2 <- krig1$map
return(krig2)
}

# And do it "safely"
Krig_function_safe<-safely(Krig_function) # skip the NAs without breaking the code

# plot map function
V_krig_map<-function(datakrig=preds, Min = min, Max = max){
  
  Min = as_tibble(Min)
  Max = as_tibble(Max)
  
  ggmap(VarariBaseMap)+
    geom_point(data=datakrig, aes(x=x, y=y, colour=pred), size=4, alpha=0.5) + 
    # geom_point(data = VData, aes(x=lon, y=lat))+
    scale_color_viridis_c(" ", option = "plasma", limits = c(Min,Max))+
    coord_sf() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +  
    theme(panel.grid.major = element_line(color = 'white', linetype = "dashed",size = 0.5),
          plot.background=element_rect(fill='white'))
 #   ggtitle(paste("Varari",DN, TD))
}
# nest by all parameters, tides, day/Night, Date, etc to make it easy to plot all types of maps
# Varari
Varari_kriging<-AllChemData %>%
  select(-Temperature)%>% # this is temporary until we get the temperature data entered
  filter(Plate_Seep == "Plate", # only plot the plates because the seep samples skew the maps
         Location == "Varari") %>%
  pivot_longer(cols = Salinity:Ammonia_umolL, names_to = "Parameters", values_to = "Values") %>%
  select(lat, lon, Tide, Day_Night, Date, Parameters, Values) %>% # select the values that are important for the kriging
  group_nest(Tide, Day_Night, Date, Parameters) %>% # the parameters to group by
  left_join(min_max)%>% # add in the mins and max values for the plots
  mutate(preds = map(data, ~Krig_function_safe(poly = V_kml)), # run the function for every nested group
         preds = map(preds, head, -1), # remove the error column
         preds = map(preds, flatten_df), # flatten back to a tibble 
 # mutate(preds = unlist(preds))
         plots = pmap(c(preds,min, max),~V_krig_map(datakrig = .x, Min = min, Max = max))) # make a plot for every nested group
 
Cabral_kriging<-AllChemData %>%
  select(-Temperature)%>%
  filter(Plate_Seep == "Plate",
         Location == "Cabral") %>%
  pivot_longer(cols = Salinity:Ammonia_umolL, names_to = "Parameters", values_to = "Values") %>%
  select(lat, lon, Tide, Day_Night, Date, Parameters, Values) %>%
  group_nest(Tide, Day_Night, Date, Parameters) %>%
  mutate(preds = map(data, ~Krig_function_safe(poly = C_kml)))

# calculate the min and max of the data ranges
min <- DataRange %>%
  filter(Location == "Varari") %>%
  select(Silicate_umolL) %>%
  slice(1) %>%
  as.matrix()

max <- DataRange %>%
  filter(Location == "Varari") %>%
  select(Silicate_umolL) %>%
  slice(2)%>%
  as.matrix()


