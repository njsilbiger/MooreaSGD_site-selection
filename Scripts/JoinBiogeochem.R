### Script to join all the biogeochemistry data from August! ##
### Nyssa Silbiger
### 10/22/2021
### updated 1/24/2021

####### load libraries #####
library(here)
library(tidyverse)
library(lubridate)

## Read in files ###

Carb<-read_csv(here("Data","August2021","CarbonateChemistry","pHProbe_Data_calculated_POcorrect.csv"))
Nuts <- read_csv(here("Data","August2021","Nutrients", "Nutrients_Watersampling_Aug21.csv")) %>%
  mutate(Date = mdy(Date))
Sites<-read_csv(here("Data","Sandwich_Locations_Final.csv"))
fDOM<-read_csv(here("Data","August2021","fDOM","Moorea_SGD_2021_fDOM.csv"))%>%
  mutate(Date = mdy(Date))


### join everything #####

AllChemData1 <- Sites %>%
  full_join(Carb)  ## Need to do this in two steps because otherwise some of the lat/longs are missing because of unequal samples between the carb and nutrient data

AllChemData<-Sites %>%
  full_join(Nuts) %>%
  full_join(AllChemData1)%>%
  full_join(fDOM)%>%
   mutate(#Date = mdy(Date),
         #SamplingTime = hms(SamplingTime),
         DateTime = ymd_hms(paste(Date,SamplingTime))) %>%
  select(Location,lat, lon, CowTagID, Top_Plate_ID, Bottom_Plate_ID, Jamie_Plate_ID, 
         Plate_Seep, Date, Time = SamplingTime, DateTime, Tide, Day_Night, Salinity,
         Temperature = TempInSitu, TA,pH, Phosphate_umolL, Silicate_umolL, 
         NN_umolL = Nitrite_umolL, Ammonia_umolL,M_C,	HIX,	MarineHumic_Like,
         VisibleHumidic_Like,	Tryptophan_Like,	Tyrosine_Like,	Lignin_Like) 


## Because there are 3 samples that have nutrient data, but not TA/pH data, they are missing dates and times in the join.  I am adding them manually below

AllChemData %>%
  filter(is.na(Time))

AllChemData$DateTime[AllChemData$CowTagID=="V3"& AllChemData$Tide =="High"& AllChemData$Day_Night == "Night"]<-mdy_hms("8/5/2021 00:00:00")
AllChemData$Time[AllChemData$CowTagID=="V3"& AllChemData$Tide =="High"& AllChemData$Day_Night == "Night"]<-"00:00:00"

AllChemData$DateTime[AllChemData$CowTagID=="V7"& AllChemData$Tide =="High"& AllChemData$Day_Night == "Night"]<-mdy_hms("8/5/2021 00:00:00")
AllChemData$Time[AllChemData$CowTagID=="V7"& AllChemData$Tide =="High"& AllChemData$Day_Night == "Night"]<-"00:00:00"

AllChemData$DateTime[AllChemData$CowTagID=="C12"& AllChemData$Tide =="Low"& AllChemData$Day_Night == "Night"]<-mdy_hms("8/9/2021 19:00:00")
AllChemData$Time[AllChemData$CowTagID=="C12"& AllChemData$Tide =="Low"& AllChemData$Day_Night == "Night"]<-"19:00:00"

AllChemData$DateTime[AllChemData$CowTagID=="VSEEP"& AllChemData$Tide =="Low"& AllChemData$Day_Night == "Night"][1]<-mdy_hms("8/8/2021 18:30:00")
AllChemData$Time[AllChemData$CowTagID=="VSEEP"& AllChemData$Tide =="Low"& AllChemData$Day_Night == "Night"][1]<-"18:30:00"


# The offshore sample is probably not good because it was sitting in the heat... the TA values dont make a lot of sense so removing here
AllChemData<-AllChemData %>%
  filter(CowTagID !="Offshore")

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


AllChemData %>%
  filter(Plate_Seep == "Plate") %>%
  ggplot(aes(y = pH, x = Silicate_umolL))+
  geom_point(aes(color = Day_Night, shape = Tide))+
  facet_wrap(~Location, scales = "free")+
  theme_bw()


AllChemData %>%
  filter(Plate_Seep == "Seep") %>%
  ggplot(aes(y = pH, x = Tide))+
  geom_boxplot(aes(color = Day_Night))+
  facet_wrap(~Location, scales = "free")+
  theme_bw()
