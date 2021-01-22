# One-point Conductivity Calibrations for the HOBO CT Loggers using raw data off of HOBOware

# https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf

# created: 9-23-2020 by Danielle Barnas
# modified: 1-22-2021

#### Conductivity Calibration for Drift #####

rm(list=ls())

library(tidyverse)
library(lubridate)
library(gsw)
library(here)

here()


###################################
# File Paths and Serial Numbers
###################################

file.date<-'0118-2021' # Date in the logger file's name
serial<-'331' # CT Probe Serial Number

condCal<-read_csv(paste0("Data/Cond_temp/Calibration/Full_Cal_",serial,".csv"))%>%
  select(-'#')%>%
  mutate(Serial=serial)
#cal.date<-'010921' # Date of logger calibration or Name of Full Calibration File

# COMMENT OUT ALL BUT THE APPROPRIATE SERIAL'S LINE
#addition
#  std.drift<- 5345.74 # 316
#  std.drift<- 7678.08 # 318
# std.drift<- 12550.87 # 319
# std.drift<- -3582.85 # 324
std.drift<- 856.83 # 331
# std.drift<- 10195.47 # 337
#  std.drift<- 12838.72 # 338
# std.drift<- 9287.55 # 343
# std.drift<- 9630.78 # 354

#multiplication
# std.drift<- 0.881493669 # 316
# std.drift<- 0.841491836 # 318
# std.drift<- 0.738484641 # 319
# std.drift<- 1.105934596 # 324
# std.drift<- 0.977420492 # 331
# std.drift<- 0.793294243 # 337
# std.drift<- 0.720586817 # 338
# std.drift<- 0.804038133 # 343
# std.drift<- 0.792957826 # 354



###################################
# Date and Time
###################################
### Maintain date time format "YYYY-MM-DD HH:MM:SS"

# Date of pre-deployment calibrations
startCal1<-'2021-01-18 06:30:00' # Y-M-D H:M:S Calibration for One-Point Cal (50.0 mS/cm)
endCal1<-'2021-01-18 06:38:00'

# Date of post-deployment calibrations
startCal2<-'2021-01-21 12:30:00' # Y-M-D H:M:S Calibration for One-Point Cal (50.0 mS/cm)
endCal2<-'2021-01-21 13:40:00'

# common garden
CGstart<-'2021-01-18 07:56:00'
CGend<-'2021-01-18 08:06:00'

# Date of in situ logs
Launch<-'2021-01-18 12:30:00'
Retrieval<-'2021-01-20 11:00:00'

###################################
# Conductivity Calibration Standards and Logging Interval
###################################

# One-Point Calibration Standard
oneCal<-50000 # uS/cm

###################################
# Pressure data
###################################
# COMMENT OUT ONE OF THE FOLLOWING

### If pairing with Pressure/Depth Logger data
Serial.depth<-'877' # Serial number of paired hobo pressure logger

### If data were recorded at a consistent pressure (bar)
#Pres_bar<-0

#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

############################################################
############################################################
### Read in and Calibration and Logger Files

# Conductivity Calibration files
#path.Cal<-'Data/Cond_temp/Calibration'
#file.names.Cal<-basename(list.files(path.Cal, pattern = c(cal.date,"csv$"), recursive = F)) #list all csv file names in the folder and subfolders
#condCal <- file.names.Cal %>%
#  map_dfr(~ read_csv(file.path(path.Cal, .),skip=1,col_names=TRUE))
#condCal<-condCal%>% # Filter specified probe by Serial number
#  select(contains('Date'),contains(Serial))%>%
#  mutate(Serial=Serial)%>%
#  rename(date=contains("Date"),TempInSitu=contains("Temp"),E_Conductivity=contains("High Range"))%>%
#  drop_na()
#condCal$date<-condCal$date%>%parse_datetime(format = "%m/%d/%y %H:%M:%S %p", na = character(), locale = default_locale(), trim_ws = TRUE) # Convert 'date' to date and time vector type

# In Situ Conductivity files
path.Log<-paste0('Data/Cond_temp')
file.names.Log<-basename(list.files(path.Log, pattern = c(file.date,"csv$"), recursive = F)) #list all csv file names in the folder and subfolders
condLog <- file.names.Log %>%
  map_dfr(~ read_csv(file.path(path.Log, .),skip=1,col_names=TRUE))
condLog<-condLog%>% # Filter specified probe by Serial number
  select(contains('Date'),contains(serial),-contains("Low"))%>%
  mutate(Serial=serial)%>%
  rename(date=contains("Date"),TempInSitu=contains("Temp"),E_Conductivity=contains("High Range"))%>%
  drop_na()
condLog$date<-condLog$date%>%parse_datetime(format = "%m/%d/%y %H:%M:%S %p", na = character(), locale = default_locale(), trim_ws = TRUE) # Convert 'date' to date and time vector type

# Parse date filters into date and type vector types
startCal1<-startCal1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endCal1<-endCal1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
startCal2<-startCal2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endCal2<-endCal2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
CGstart<-CGstart%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
CGend<-CGend%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

# Filter out dates
condCal1<-condCal%>%filter(between(date,startCal1,endCal1))
condCal2<-condCal%>%filter(between(date,startCal2,endCal2))
condLog<-condLog%>%filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval)) 
condCal<-union(condCal1,condCal2) # Join calibration files together
CT.data<-union(condCal,condLog)# Join Calibration and Logged files
#CT.data$date<-CT.data$date - seconds(5) # offset time if necessary

############################################################
# Load Pressure Data from HOBO Pressure Loggers for In Situ Practical Salinity Calculations

If(Serial.depth = TRUE) {
  path.depth<-'Data/Depth/Calibrated_files'
  file.names.depth<-basename(list.files(path.depth,pattern = c(Serial.depth, file.date,"csv$"), recursive = F))
  Depth.data <- file.names.depth %>%
    map_dfr(~ read_csv(file.path(path.depth, .),col_names=TRUE))
  Depth.data<-Depth.data%>%
    filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval))%>%
    rename(Serial.depth=Serial,TempInSitu.depth=TempInSitu)%>%
    mutate(AbsPressure_bar=AbsPressure*0.01) # convert kPa to Bar (conversion: 1 kPa = 0.01 Bar)
} else {
  Depth.data<-tibble(date=CT.data$date, AbsPressure_bar=Pres_bar)
}
CT.data<-CT.data%>% # amend to larger dataframe
  left_join(Depth.data,by='date')

############################################################
## Calculate temperature correction factor (f25) using ISO-7888 (1985)
# source: https://cdn.standards.iteh.ai/samples/14838/ffffe623f2654f4ea96ea3ceb22de16f/ISO-7888-1985.pdf
# nonlinear temperature compensation

# Constants
a<-as.numeric('0.962144')
n<-as.numeric('0.965078')
A<-as.numeric('-0.198058')
B<-as.numeric('-1.992186')
C<-as.numeric('231.17628')
D<-as.numeric('86.39123')

# Calculate viscosity and f25, then calculate temperature compensated conductivity (Specific Conductance) through equation: SC = EC * f25
CT.data<-CT.data%>%
  mutate(vis = A + exp(B + (C / (TempInSitu + D))))%>%
  mutate(f25 = ((1 - a) + a * ((vis)^n)) * 1.116)%>%
  mutate(Sp_Conductance = E_Conductivity * f25)

# one-point calibration of Specific Conductance
theta<-CT.data%>%
  filter(between(date,startCal1,endCal1))%>%
  summarise(mean(TempInSitu))%>%
  as.numeric()
Cond_Ref<-1060*theta+23500 # actual calibration solution conductance at logged temperature at pre-deployment

cal.offset<-CT.data%>%
  filter(between(date,startCal1,endCal1))%>%
  summarise(Cond_Ref - mean(Sp_Conductance + std.drift))%>%
  as.numeric()

CT.data<-CT.data%>%
  mutate(Sp_Conductance.cal=Sp_Conductance + std.drift + cal.offset)

############################################################
## Correct for drift between pre- and post-deployment calibrations

meanCal1<-CT.data%>%
  filter(between(date,startCal1,endCal1))%>%
  summarise(mean(Sp_Conductance))%>%
  as.numeric()
meanCal2<-CT.data%>%
  filter(between(date,startCal2,endCal2))%>%
  summarise(mean(Sp_Conductance))%>%
  as.numeric()
drift.off<-meanCal1-meanCal2
drift.corr=drift.off/length(condLog$date)

condLog<-CT.data%>%
  filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval))%>%
  mutate(drift.correction.new=drift.corr)%>%
  mutate(drift.correction=cumsum(drift.correction.new))%>%
  select(-drift.correction.new)
condLog<-condLog%>%
  mutate(Sp_Conductance.drift = Sp_Conductance.cal + drift.correction)
condCal1b<-CT.data%>%
  filter(between(date,startCal1,endCal1))%>%
  mutate(Sp_Conductance.drift = Sp_Conductance.cal)
condCal2b<-CT.data%>%
  filter(between(date,startCal2,endCal2))%>%
  mutate(Sp_Conductance.drift = Sp_Conductance.cal + drift.off)
condCal<-union(condCal1b,condCal2b)%>%
  mutate(AbsPressure_bar = 0) # Join calibration files together
CT.data<-full_join(condCal,condLog)# Join Calibration and Logged files

############################################################
# Calculate Salinity using gsw package for the PSS-78 equation

CT.data<-CT.data%>%
  mutate(Sp_Cond_mS.cm=Sp_Conductance.drift*0.001)%>% # Sp_Conductance in mS/cm
  mutate(SalinityInSitu_1pCal=gsw_SP_from_C(C = Sp_Cond_mS.cm, t = 25, p=AbsPressure_bar)) # Use PSS-78 Equations for Salinity calculation

############################################################
# Graph data

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>24)%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal,color=TempInSitu))+
  geom_line()

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=-Depth,color=SalinityInSitu_1pCal))+
  geom_line()+
  scale_colour_gradient(high = "#132B43",low = "#56B1F7")

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=-Depth,color=TempInSitu))+
  geom_line()

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>24)%>%
  ggplot(aes(x=date,y=TempInSitu,color=SalinityInSitu_1pCal))+
  geom_line()+
  scale_colour_gradient(high = "#132B43",low = "#56B1F7")


# Write CSV file
write_csv(CT.data,paste0('Data/Cond_temp/Calibrated_files/',file.date,'_CT',serial,'_insitu_1pcal_offsetAdd.csv'))
