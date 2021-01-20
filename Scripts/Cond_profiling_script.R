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


#source the map data-------------
source(here("Scripts","cond_rad_maps.R"))

### read in the profile data

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


#### Read in the timeseries data ###########
# cond and temp
CondtimePath<-here("Data","Cond_temp","Calibrated_files", "timeseries")
files <- dir(path = CondtimePath,pattern = ".csv")

# # read in the data
CondtimeData <- files %>%
  map(~ read_csv(file.path(CondtimePath, .))) %>% 
  #   #col_names = c("ID", "date","HighRange", "Temp","SpC","Salinity")
  reduce(rbind) %>%
  mutate(date = ymd_hms(date)) %>% # make data a date_time
  select(date,Temp = TempInSitu, Salinity = SalinityInSitu_1pCal, Depth, Serial) %>%
  drop_na(Salinity) %>%
  filter(Salinity>28) %>% # remove the times the sensor jumped out of the water
  mutate(Depth = - Depth, # make depth negative so the profiles go down
         Day = day(date),
         Site = "West") %>% # pull out the day to facet by
  filter(Depth< -0.5) # one weird depth reading to remove

# pH
pHtimePath<-here("Data","pH")
files <- dir(path = pHtimePath,pattern = ".csv")

# # read in the data
pHtimeData <- files %>%
  map(~ read_csv(file.path(pHtimePath, .), skip=2)) %>% 
  #   #col_names = c("ID", "date","HighRange", "Temp","SpC","Salinity")
  reduce(rbind) %>%
  select(date = `Date Time, GMT -1000`, Temp =  `Temp, (*F)`, pH) %>%
  mutate(date = ymd_hms(date)) %>% # make data a date_time
  drop_na(pH) %>%
  filter(date > ymd_hms("2021-01-17 10:35:00"),
         date < ymd_hms("2021-01-19 9:35:00"),
  )
  
# plots  
Saltime<-CondtimeData %>%
  ggplot(aes(x = date, y = Depth, color = Salinity))+
  geom_line()+
  scale_color_gradient(low = "yellow", high = "blue") +
  theme_bw()+
  facet_wrap(~Serial)

TempTime<-CondtimeData %>%
  ggplot(aes(x = date, y = Temp, color = Temp))+
  geom_line()+
  scale_color_gradient(low = "blue", high = "red") +
  theme_bw()+
  facet_wrap(~Serial)

pHTime<- pHtimeData %>%
  ggplot(aes(x = date, y = pH, color = pH))+
  geom_line()+
  ylab('pH (NBS scale)')+
  scale_color_gradient(low = "blue", high = "purple") +
  theme_bw()+
  ggsave("Output/West_pH.pdf", width = 5, height = 5)
  

Saltime/TempTime +
  ggsave("Output/West_timeseriesCTD.pdf", width = 12, height = 6)
  