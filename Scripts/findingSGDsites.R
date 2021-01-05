### Script to select sites from nutrient data collected in Moorea on August 2020###


### Load Libraries ####
library(tidyverse)
library(here)
library(lubridate)
library(ggmap)
library(sf)
library(ggsn)
library(ggrepel)
library(geodist)

## read in data ####
path<-here::here("Data", "water_column_nutrients_Aug_2020_compiled_final.csv")
NutData<-read_csv(path) # Silicate/Nutrients data from Tom Adams
HablerData<-read_csv(here::here("Data", "Tahiti_Moorea_coordinates_Habler.csv"))## sites from Habler et al
FisherData<-read_csv(here::here("Data", "SGD_sites_Jean.csv")) ## Sites from Fishers

### Process ####


#Nutrient data
## make dates and times
NutData<-NutData %>%
  mutate(Time = hms(Time),
         Date = mdy(Date),
         DateTime = paste(Date,Time),
         HighSi = ifelse(Silicate>1.5,1,0))# label the high Si sites (2 SD above the mean)
         #HighSi = ifelse(Silicate>=mean(Silicate)+2*sd(Silicate),1,0))# label the high Si sites (2 SD above the mean)


## Habler data ##
# only select the FSGD (fresh groundwater)
HablerData <-HablerData %>%
  filter(WaterType == "FSGD") %>%
  mutate(ID = "Habler") %>%
  rename(Lon = long, ## rename columns to match with other data 
          Lat = lat) %>%
  select(Lat, Lon, ID)

## Fisher Data ####
FisherData <- FisherData %>%
 # filter(Name != "Interview -1") %>% # select sites that had a visual and were confirmed by at least two people
  mutate(ID = "Fisher") %>%
  select(Lat, Lon, ID)
  

## make a bubble plot map

#register_google(key = '') ### use your own API

# First make a map of all the silicate data
M3<-get_map('Moorea',zoom = 12, maptype = 'satellite')
Mooreamap<- ggmap(M3)+
  scalebar(x.min = -149.9, x.max = -149.7,y.min = -17.63, y.max = -17.5,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = 5)+
  geom_point(data = NutData, mapping = aes(x=Lon, y=Lat, size=Silicate, color = Silicate))+
  xlab("")+
  ylab("")+
  guides(color= guide_legend(title = expression(paste("Silicate ",mu, "mol L"^-1))), size=guide_legend(title = expression(paste("Silicate ",mu, "mol L"^-1))))+
  scale_size_continuous(limits=c(0, 7), breaks=seq(0, 7, by=1))+
  scale_color_gradient(low = "yellow", high = "red", na.value = NA,limits=c(0, 7), breaks=seq(0, 7, by=1))+
  geom_point(data = NutData %>% filter(HighSi==1),mapping = aes(x=Lon, y=Lat), color = "black" )+
  geom_label_repel(data = NutData %>% filter(HighSi==1), mapping =aes(x=Lon, y=Lat, label = Silicate), nudge_x = 0.01 )+
  ggsave(here("Output","SiMap.pdf"), height = 8, width = 8)


## export the target sites
NutData %>% filter(HighSi==1) %>% write.csv(file = here("Output", "targetsilicate.csv"), row.names = FALSE)

## Make a plot with target sites from all 3 data sets: Silicate, Habler sites, and Jean Fisher sites
TargetNut<-NutData %>% 
  filter(HighSi==1) %>%
  mutate(ID = "Nutrients") %>%
  select(Lat, Lon, ID)


## merge all the data together
AllSites<-bind_rows(TargetNut, HablerData, FisherData) %>%
  mutate(SiteID = row_number()) # add random numbers to ID the sites

## select possible overlapping sites to target
### find distance between all gps points
mm<-geodist_vec(
  AllSites$Lon,
  AllSites$Lat)

Sites2Keep<-as.tibble(which(mm < 300, arr.ind = TRUE)) %>% # find all sites that are within 200 m
  filter(row !=col) %>% # remove the ones where the sites are identical
  select(row) %>% # the row and columns are now repeats. Pull out one of the which has the row numbers associated with the site IDs
  distinct()

Mooreamap_allsites<- ggmap(M3)+
  scalebar(x.min = -149.9, x.max = -149.7,y.min = -17.63, y.max = -17.5,
           model = 'WGS84', box.fill = c("yellow", "white"), st.color = "white",
           location =  "bottomleft", transform = TRUE, dist_unit = "km", dist = 5)+
  geom_point(data = AllSites, mapping = aes(x=Lon, y=Lat, color = ID), size = 4, alpha = .60)+
  scale_color_manual(values = c("green", "orange", "white"))+
  xlab("")+
  ylab("")+
  geom_label_repel(data = AllSites %>% filter(SiteID %in% Sites2Keep$row), mapping = aes(x=Lon, y=Lat, label = SiteID), force = 2)+
  ggsave(here("Output","AllSites_labels.pdf"), height = 8, width = 8)

# priority sites to target
priority<-AllSites %>% filter(SiteID %in% Sites2Keep$row)
write_csv(x = priority, path = here::here("Output", "prioritysites.csv"))
