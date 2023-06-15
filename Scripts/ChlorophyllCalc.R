### Script to read in chlorophyll data from the Synergy 96 well plate in the Molecular Lab ####
### By Nyssa Silbiger ###
### Created on 6/14/2023 #####


### load libraries ######33
library(tidyverse)
library(lubridate)
library(here)
library(janitor)

### read in data ####
PlateData<-read_tsv(here("Data","June2023","Chlorophyll","Chl_plankton_230614_.txt"), skip = 47) %>%
  clean_names(case = "upper_camel")

# metadata
MetaData<-read_csv(here("Data","June2023","Chlorophyll","Metadata_20230614.csv"))

##### Analysis ####

# Bring together the plate and metadata
Data_combined<-MetaData %>% 
  left_join(PlateData) %>%
  drop_na() %>% # drop all the empty plates
  # Calculate chl from Jeffrey and Humphrey (1975)
  # units in mg m-3
  mutate(Chla = (11.85*(X664-X750) - 1.54*(X647 - X750) - 0.08* (X630-X750))*SampleVolume_ml/Path_cm*FilterVolume_L,
         Chlb = (-5.43*(X664 - X750) + 21.03* (X647 - X750) - 2.66*(X630-X750))*SampleVolume_ml/Path_cm*FilterVolume_L,
         Chlc = (-1.67*(X664 - X750) - 7.60*(X647 - X750) + 24.52 *(X630-X750))*SampleVolume_ml/Path_cm*FilterVolume_L)

# Sample volume =  extraction volume in ml
# Path_cm = Path length
# Filter Volume = Filtered volume of water in L

## Normalized data ###
Data_norm <- Data_combined %>% 
  group_by(SampleID)%>%
  summarise_at(vars(Chla,Chlb,Chlc), .funs = mean) %>% # average the triplicate data
  ungroup()%>%
  reframe(SampleID = SampleID,
          Chla_norm = Chla - Chla[SampleID == "Blank"], # subtract the blank data
          Chlb_norm = Chlb - Chlb[SampleID == "Blank"],
          Chlc_norm = Chlc - Chlc[SampleID == "Blank"],
          Chl_total = Chla_norm + Chlb_norm +Chlc_norm
  ) %>%
  filter(SampleID != "Blank") # remove the blanks and keep the samples
  
  
