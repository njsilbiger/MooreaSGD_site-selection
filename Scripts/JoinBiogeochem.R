### Script to join all the biogeochemistry data from August! ##
### Nyssa Silbiger
### 10/22/2021

####### load libraries #####
library(here)
library(tidyverse)

## Read in files ###

Carb<-read_csv(here("Data","August2021","CarbonateChemistry","pHProbe_Data.csv"))
Nuts <- read_csv(here("Data","August2021","Nutrients", "Nutrients_Watersampling_Aug21.csv"))
Sites<-read_csv(here("Data","Sandwich_Locations_Final.csv"))


### join everything #####

AllChemData <- Carb %>%
  full_join(Nuts) %>%
  full_join(Sites)

## Some plots

Carb %>%
  filter(Location != "Offshore") %>%
ggplot(aes(x = Salinity_In_Lab, y = TA))+
  geom_point(aes(color = Day_Night))+
  facet_wrap(~Location)
