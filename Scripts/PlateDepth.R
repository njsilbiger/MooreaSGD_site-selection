### PROCESSING DEPTH TO PLATES AND CTs
### Created by Danielle Barnas
### Created on 9/28/2022


#### LOAD LIBRARIES ####
library(tidyverse)
library(here)
library(lubridate)


#### READ IN DATA ####
depth <- read_csv(here("Data","Sandwich_Depth.csv"))
Vsled <- read_csv(here("Data","Biogeochem","July2022","Vsled_WL_06032022.csv"))
Csled <- read_csv(here("Data","Biogeochem","July2022","Cabral_WL_06232022.csv"))

#### PROCESS RAW DEPTH DATA ####

# round all depth times to nearest 2 minutes
depth <- depth %>%
  mutate(TimeB = if_else(minute(Time) %% 2 == 0, hms(Time), hms(Time) + minutes(1))) %>% # add 1 minute to odd time stamps to match sled times
  mutate(TimeB = if_else(CowTagID == 'C12', hms("12:00:00"), hms(TimeB))) %>% # modify from 11:60:00 to 12:00:00
  mutate(TimeB = if_else(CowTagID == 'V20', hms("07:50:00"), hms(TimeB))) %>% # sled does not have data until 2 minutes after V20 depth recording
  unite(Date, TimeB, col = 'DateTime', sep = " ", remove = T) %>% # unite date and time
  select(-Time) %>%
  mutate(DateTime = mdy_hms(DateTime))

Vsled <- Vsled %>%
  unite(Date, Time, col = 'DateTime', sep = " ", remove = T) %>% # unite date and time
  mutate(DateTime = ymd_hms(DateTime)) %>%
  mutate(Depth_cm = Depth * 100) %>% # calculate Depth in cm
  select(-Depth) %>%  # remove original depth column
  mutate(Location = "Varari")

Csled <- Csled %>%
  rename(DateTime = date) %>%
  mutate(DateTime = ymd_hms(DateTime)) %>%
  mutate(Depth_cm = Depth * 100) %>% # calculate Depth in cm
  select(-Depth) %>%  # remove original depth column
  mutate(Location = "Cabral") %>%
  arrange(DateTime)

# bind sled data together
Sleds<- rbind(Vsled, Csled)

# join dataframes based on concurrent CT-Seep depth times
fullDepth <- left_join(depth, Sleds) %>%
  filter(CowTagID != "CSEEP") %>%
  group_by(Location) %>%
  arrange(DateTime) %>% # make sure values are in chronological order for tidal variation
  ungroup() #%>%
  #select(CowTagID, Location, Dist_CT_cm, Depth_cm)  # only keep relevant data

# mean depth of sled CT at each seep during depth survey intervals
VseepDepth <- fullDepth %>% filter(Location == "Varari")
VseepDepth <- VseepDepth[1,'DateTime']
VseepDepth <- Vsled %>%
  filter(DateTime == VseepDepth$DateTime[1]) %>%
  select(Location, Depth_cm) %>%
  mutate(CowTagID = "VSEEP") %>%
  mutate(Dist_CT_cm = Depth_cm, Tidal_diff = 0)

CseepDepth <- fullDepth %>% filter(Location == "Cabral")
CseepDepth <- CseepDepth[1,'DateTime']
CseepDepth <- Csled %>%
  filter(DateTime == CseepDepth$DateTime[1]) %>%
  select(Location, Depth_cm) %>%
  mutate(CowTagID = "CSEEP") %>%
  mutate(Dist_CT_cm = Depth_cm, Tidal_diff = 0)

#### CALCULATE TIDAL DIFFERENCES ####

# identify first value as having zero tidal difference
firstval <- tibble(value = as.numeric(0))

# parse tidal differences vector into tibble for each site
Vdiffs <- fullDepth %>% filter(Location == 'Varari')
Vdiffs <- as_tibble(diff(Vdiffs$Depth_cm, lag = 1))

Cdiffs <- fullDepth %>% filter(Location == 'Cabral')
Cdiffs <- as_tibble(diff(Cdiffs$Depth_cm, lag = 1))

# rbind tidal differences, with firstval on top
Vdiffs <- rbind(firstval, Vdiffs)
Cdiffs <- rbind(firstval, Cdiffs)
diffs <- rbind(Vdiffs, Cdiffs)

# bind with full dataframe in same order as original dataframe
fullDepth <- fullDepth %>%
  cbind(diffs) %>%
  rename(Tidal_diff = value) %>%
  bind_rows(VseepDepth, CseepDepth) %>%
  mutate(adj_CT_depth_cm = Dist_CT_cm - Tidal_diff) %>%  # adjusted CT depths with tidal differences subtracted
  select(Location, CowTagID, Dist_CT_cm, Depth_cm, Tidal_diff, adj_CT_depth_cm)

# write csv
write_csv(fullDepth, here("Data", "Adj_Sandwich_Depth.csv"))






