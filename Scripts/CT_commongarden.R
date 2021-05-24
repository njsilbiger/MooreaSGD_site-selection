### Script to look at common garden from 2021-05-22 to 23 ####
### We put all the sensors on the flow through water tables at Gump and messed with them in different ways to see what happens####
### Made by Nyssa Silbiger #####
### Created on 5/23/2021 #####


######### load libraries ##############
library(tidyverse)
library(lubridate)
library(viridis)
library(stringr)
library(ggrepel)
library(here)



##### read in data ###

# read in the benthic data
file_loc<-here("Data","May2021","Cond_temp","Csv_files", "QC", "commongarden")


files <- dir(path = file_loc,pattern = ".csv")


# read in the cond data ####
GardenData <- files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(~ read_csv(file.path(file_loc, .)),.id = "filename") %>%
  select(date, Serial, TempInSitu, Salinity) %>%
  mutate(Serial_num = str_extract(Serial,"[0-9]+"))
  


### Extract sesnors that Danielle added sediment to

sed_sensor<-c("344","327","326","346")


GardenData %>%
  filter(Serial_num == sed_sensor) %>%
  ggplot(aes(x = date, y = Salinity, color = TempInSitu, group = Serial_num))+
  geom_line()+
  geom_vline(xintercept = mdy_hms("05/22/2021 17:50:00"), lwd = 2)


## look at all data without sediment or anything else
GardenData %>%
  filter(Serial_num != sed_sensor) %>%
  filter(date > mdy_hms("05/22/2021 14:00:00") & date < mdy_hms("05/23/2021 12:00:00") ) %>%
  ggplot(aes(x = date, y = Salinity, group = Serial_num, color = Serial_num))+
  geom_line()+
  geom_text(data = GardenData %>% filter(date == last(date)), aes(label = Serial_num, 
                                                               x = date, 
                                                               y = Salinity, 
                                                               color = Serial_num)) + 
  guides(color = FALSE) + theme_bw()
  
  