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

#create new column with biomass
Biomass.Info <- Biomass.Info %>%
  mutate(Biomass = Total_Weight-Boat_Weight)

#filter out sand
Biomass.Info <- filter(Biomass.Info, !grepl("SAND",Group))

#separate Tile into 2 columns: PlateID and Other
Biomass.Info <- separate(Biomass.Info, Tile, c("PlateID", "Other"), -2)

#Remove Other column
Biomass.Info <- select(Biomass.Info, -Other)

#consolidate duplicate rows 
Biomass.Info <- aggregate(Biomass ~ PlateID, data=Biomass.Info, sum)

#export total biomass in csv file
write_csv(Biomass.Info,here("Data","August2021","CommunityRespoData","TotalBiomass.csv") )

#Merge these data into the P/R script, use right join
#Or write directly into Sample.Info?




#slimline code #not working
Biomass.Total <- Biomass.Info %>% 
  select(Biomass.Info, -notes) %>%  #delete column notes
  colnames(Biomass.Info) <- c("Tile","Group", "Total_Weight","Boat_Weight") %>% #Change column names
  mutate(Biomass = Total_Weight-Boat_Weight) %>% #create new column with biomass
  filter(Biomass.Info, !grepl("SAND",Group)) %>%  #filter out sand
  separate(Biomass.Info, Tile, c("PlateID", "Other"), -2) %>%  #separate Tile into 2 columns: PlateID and Other
  select(Biomass.Info, -Other) %>%  #delete Other column
  aggregate(Biomass ~ PlateID, data=Biomass.Info, sum) #consolidate duplicate rows
