### Script to join all the biogeochemistry data from August! ##
### Nyssa Silbiger
### 10/22/2021

####### load libraries #####
library(here)
library(tidyverse)
library(lubridate)

## Read in files ###

Carb<-read_csv(here("Data","August2021","CarbonateChemistry","pHProbe_Data_calculated.csv"))
Nuts <- read_csv(here("Data","August2021","Nutrients", "Nutrients_Watersampling_Aug21.csv"))
Sites<-read_csv(here("Data","Sandwich_Locations_Final.csv"))


### join everything #####

AllChemData <- Carb %>%
  full_join(Nuts) %>%
  full_join(Sites) %>%
  mutate(Date = mdy(Date),
         #SamplingTime = hms(SamplingTime),
         DateTime = ymd_hms(paste(Date,SamplingTime))) %>%
  select(Location,lat, lon, CowTagID, Top_Plate_ID, Bottom_Plate_ID, Jamie_Plate_ID, Plate_Seep, Date, Time = SamplingTime, DateTime, Tide, Day_Night, Salinity,Temperature = TempInSitu, TA, Phosphate_umolL, Silicate_umolL, NN_umolL = Nitrite_umolL, Ammonia_umolL ) 

write_csv(AllChemData ,here("Data","August2021","Allbiogeochemdata_QC.csv"))

## Some plots

AllChemData %>%
  filter(Location != "Offshore",
         Plate_Seep %in% c("Plate","Seep","Spring")) %>%
  ggplot(aes(x = log(Silicate_umolL), y = log(NN_umolL)))+
  geom_point(aes(color = Plate_Seep))+
 # geom_smooth(method = "lm")+
  facet_wrap(~Location, scales = "free")+
  theme_bw()

AllChemData %>%
  filter(Location != "Offshore",
         Plate_Seep %in% c("Plate","Seep","Spring")) %>%
  ggplot(aes(x = Silicate_umolL, y = Salinity))+
  geom_point(aes(color = Plate_Seep))+
  geom_smooth(method = "lm")+
  facet_wrap(~Location, scales = "free")+
  theme_bw()

AllChemData %>%
  filter(Location != "Offshore") %>%
  ggplot(aes(x = Salinity, y = TA))+
  geom_point(aes(color = Plate_Seep))+
  facet_wrap(~Location, scales = "free")+
  theme_bw()
