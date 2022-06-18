## process pH for Orion
# Created by Nyssa Silbiger
# Edited on 07/27/2021

library(tidyverse)
library(seacarb)
library(broom)
library(here)
library(lubridate)

## bring in pH calibration files and raw data files
pHcalib<-read_csv(here("Data","March2022","CarbonateChemistry","TrisCalibrationLog.csv")) %>%
  mutate(TrisCalDate = mdy(TrisCalDate))
# pHData<-read_csv(here("Data","August2021","CarbonateChemistry","pHProbe_Data.csv"))
pHData<-read_csv(here("Data","March2022","CarbonateChemistry","pHProbe_Data.csv"))%>%
  mutate(TrisCalDate = mdy(TrisCalDate),
         Date = mdy(Date))
# pHData<-read_csv(here("Data","August2021","CarbonateChemistry","pHProbe_SEEP_2022_02_10.csv"))%>%
#   mutate(TrisCalDate = mdy(TrisCalDate))

# Needed for phosphate data
NutData<-read_csv(here("Data","March2022","Nutrients","Nutrients_watersampling_Mar22.csv")) %>%
  select(CowTagID, SeepCode, Day_Night, Tide, Date, Phosphate_umolL, NN_umolL=Nitrite_umolL, Ammonia_umolL, Silicate_umolL) %>%
  mutate(Date = mdy(Date))

pHData<-left_join(pHData,NutData) 



## take the mV calibration files by each date and use them to calculate pH
pHSlope<-pHcalib %>%
  filter(HOBO_Orion =="Orion") %>% # extract only the orion data
  nest_by(TrisCalDate)%>%
  mutate(fitpH = list(lm(mVTris~TTris, data = data))) %>% # linear regression of mV and temp of the tris
  summarise(broom::tidy(fitpH)) %>% # make the output tidy
  select(TrisCalDate, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%# put slope and intercept in their own column
  right_join(.,pHData) %>% # join with the pH sample data
  mutate(mVTris = TempInLab*TTris + `(Intercept)`) %>% # calculate the mV of the tris at temperature in which the pH of samples were measured
  mutate(pH = pH(Ex=mV,Etris=mVTris,S=Salinity,T=TempInLab)) %>% # calculate pH of the samples using the pH seacarb function
  drop_na(TempInSitu)%>%
  drop_na(mV)

# The TA data is missing from some bottles because of lack of water, but it really doesnt affect the pH calc.
# I am replacing the missing values with 2300 for the pH calculation then converting it back to NA

NoTA<-which(is.na(pHSlope$TA))

pHSlope$TA[NoTA]<-2300

NoPO<-which(is.na(pHSlope$Phosphate_umolL))
pHSlope$Phosphate_umolL[NoPO]<-0

# If the salinity is missing from the TA measurements take the salinity from the field
pHSlope$Salinity<-ifelse(is.na(pHSlope$Salinity), pHSlope$Salinity_In_Lab, pHSlope$Salinity)


#Now calculate pH
pHSlope <-pHSlope%>%
  mutate(pH_insitu = pHinsi(pH = pH, ALK = TA, Tinsi = TempInSitu, Tlab = TempInLab, 
                            S = Salinity,Pt = Phosphate_umolL, k1k2 = "m10", kf = "dg")) %>%
  select(!pH) %>% # I only need the in situ pH calculation so remove this
  rename(pH = pH_insitu) %>% # rename it 
  ungroup() %>%
  select(Date, CowTagID,SeepCode, Tide, Day_Night, SamplingTime,Salinity=Salinity, pH, TempInSitu, TA, NN_umolL, Silicate_umolL, Ammonia_umolL, Phosphate_umolL, Notes) # keep what I want

pHSlope$TA[NoTA]<-NA # make TA na again for the missing values

  #select(Date, CowTagID,Tide, Day_Night, SamplingTime,Salinity,pH, pH_insitu, TempInSitu) ## need to calculate pH insi then it is done

## write the data
write_csv(x = pHSlope, file = here("Data","March2022","CarbonateChemistry","pHProbe_Data_calculated_POcorrect.csv"))
