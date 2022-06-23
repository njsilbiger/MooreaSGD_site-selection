# calculate carbonate parameters

# load libraries
library(tidyverse)
library(seacarb)
library(here)


# read in data
Cdata_orig<-read_csv(here("Data","August2021","Allbiogeochemdata_QC.csv"))

# only run cases where both pH and TA are present of the code does not work
Cdata<- Cdata_orig %>%
  drop_na(TA,pH)

# calculate carbonate parameters
CO2<-carb(flag=8, Cdata$pH, Cdata$TA/1000000, S=Cdata$Salinity, 
          T=Cdata$Temperature, Patm=1, P=0, Pt=Cdata$Phosphate_umolL/1000000, Sit=Cdata$Silicate_umolL/1000000, k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential")

#TA is divided by 1000000 because all calculations are in mol/kg in the seacarb package

# calculate error propogation
er<-errors(flag=8, Cdata$pH, Cdata$TA/1000000, 
           S=Cdata$Salinity, T=Cdata$Temperature, 
           Patm=1, P=0,Pt=Cdata$Phosphate_umolL/1000000,
           Sit=Cdata$Silicate_umolL/1000000,evar1 = 0.01, evar2 = 5e-6) 

#average error for DIC based on pH and TA
mean(er$DIC*1000000)
sd(er$DIC*1000000)/sqrt(nrow(er))

#convert CO2, HCO3, CO3, DIC, and Alk back to micromol for easier interpretation
CO2all<-CO2 %>%
  mutate_at(vars(c("CO2","HCO3","CO3","DIC","ALK")), .funs = list(~.*1000000)) %>% # multiple everything by 1e6
  select(!c(pH, ALK, S, T,Patm, flag, P) )%>% #remove the columns we don't need
  bind_cols(Cdata,.) # bind it with the Cdata on the RHS 

# add back in all the data with missing TA and pH values
totaldata<-left_join(Cdata_orig, CO2all)

# write the csv file
write_csv(totaldata, here("Data", "August2021","Allbiogeochem_wCarb.csv"))
