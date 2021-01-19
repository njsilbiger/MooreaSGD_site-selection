### script for plotting profiling data #####
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


#source the map data
source(here("Scripts","cond_rad_maps.R"))

### read in the profile data

# CondProfileData<-read_csv("Data/Cond_temp/Calibrated_files/Profiling/011721_CT354_1pcal.csv")%>%
#   mutate(date = ymd_hms(date)) %>% # make data a date_time
#   select(date,Temp = TempInSitu, Salinity = SalinityInSitu_1pCal, Depth) %>%
#   drop_na(Salinity) %>%
#   filter(Salinity>28) %>% # remove the times the sensor jumped out of the water
#   mutate(Depth = - Depth) # make depth negative so the profiles go down

# find the waypoints that match with the rad data

# RadData %>%
#   mutate(ID =  case_when(
#     between(Waypts$date, date, date+minutes(6) ~ Waypts$name
#   )))
CondProPath<-here("Data","Cond_temp","Calibrated_files", "Profiling")
files <- dir(path = CondProPath,pattern = ".csv")
# 
# # read in the data
 CondProfileData <- files %>%
   map(~ read_csv(file.path(CondProPath, .))) %>% 
   #   #col_names = c("ID", "date","HighRange", "Temp","SpC","Salinity")
   reduce(rbind) %>%
   mutate(date = ymd_hms(date)) %>% # make data a date_time
   select(date,Temp = TempInSitu, Salinity = SalinityInSitu_1pCal, Depth) %>%
   drop_na(Salinity) %>%
   filter(Salinity>28) %>% # remove the times the sensor jumped out of the water
   mutate(Depth = - Depth, # make depth negative so the profiles go down
          Day = day(date),
          Site = ifelse(Day==17, "Site C","Site D")) # pull out the day to facet by
# 

## Plot profiles

AllProfiles<-ggplot(CondProfileData, aes(x = date, y = Depth, col = Salinity))+
  geom_line()+
  labs(ylab = "Depth (m)")+
  scale_color_gradient(low = "yellow", high = "blue") +
  theme_bw()+
  facet_wrap(~Site, scales = "free")+
    ggsave("Output/salinityprofiles.pdf", width = 10, height = 5)



