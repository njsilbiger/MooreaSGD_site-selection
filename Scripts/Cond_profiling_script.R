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


### read in data
CondPath<-here("Data","Cond_temp","Calibrated_files", "Profiling")
files <- dir(path = CondPath,pattern = ".csv")

# read in the data
CondProfileData <- files %>%
  map(~ read_csv(file.path(CondPath, .))) %>% 
  #col_names = c("ID", "date","HighRange", "Temp","SpC","Salinity")
  reduce(rbind) %>%
  mutate(date = ymd_hms(date)) %>% # make data a date_time
  select(date,Temp = TempInSitu, Salinity = SalinityInSitu_1pCal, Depth) %>%
  drop_na(Salinity) %>%
  filter(Salinity>28) %>% # remove the times the sensor jumped out of the water
  mutate(Depth = - Depth) # make depth negative so the profiles go down


## Plot profiles

AllProfiles<-ggplot(CondProfileData, aes(x = date, y = Depth, col = Salinity))+
  geom_line()+
  labs(ylab = "Depth (m)")+
  theme_bw()+
  ggsave("Output/salinityprofiles.pdf", width = 6, height = 6)

