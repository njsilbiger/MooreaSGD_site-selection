#Code for total biomass for plate community P/R
#Created by Hendrikje Jorissen

#Read in required libraries
library(tidyverse)
library(here)

#Load biomass data 
Biomass.Info <- read.csv(file = here("Data","August2021","CommunityRespoData","AlgalBiomassMetadata.csv"))

#remove notes column
Biomass.Info <-select(Biomass.Info, -notes)

#Change column names
colnames(Biomass.Info) <- c("Tile","Group", "Total_Weight","Boat_Weight")

#Get total biomass for each tile
Biomass.Total <- Biomass.Info %>% 
  mutate(Biomass = Total_Weight-Boat_Weight) %>% #create new column with biomass
  filter(!grepl("SAND",Group)) %>%  #filter out sand
  separate(Tile, c("PlateID", "Other"), -2) %>%  #separate Tile into 2 columns: PlateID and Other
  select(-Other) %>%  #delete Other column
  group_by(PlateID) %>% #group duplicate plates
  summarise(Biomass=sum(Biomass)) #summarize duplicate rows


