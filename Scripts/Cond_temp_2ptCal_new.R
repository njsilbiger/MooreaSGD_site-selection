# Two-point Conductivity Calibrations for the HOBO CT Loggers using raw data off of HOBOware

# https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf

# created: 9-23-2020 by Danielle Barnas
# modified: 1-25-2021

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

file.date<-'0117-1921' # Date in the logger file's name
serial<-'338' # CT Probe Serial Number

condCal<-read_csv(paste0("Data/Cond_temp/Calibration/Full_Cal_",serial,".csv"))%>%
  select(-'#')%>%
  mutate(Serial=serial)
#cal.date<-'010921' # Date of logger calibration or Name of Full Calibration File

###################################
# Date and Time
###################################
### Maintain date time format "YYYY-MM-DD HH:MM:SS"

# HIGH CALIBRATION POINT
# Date of pre-deployment calibrations
startHigh1<-'2021-01-17 08:29:00' # Y-M-D H:M:S Calibration for One-Point Cal (50.0 mS/cm)
endHigh1<-'2021-01-17 08:31:00'

# Date of post-deployment calibrations
startHigh2<-'2021-01-19 13:40:00' # Y-M-D H:M:S Calibration for One-Point Cal (50.0 mS/cm)
endHigh2<-'2021-01-19 13:45:00'

# LOW CALIBRATION POINT
# Date of pre-deployment calibrations
startLow1<-'2021-01-17 08:35:00' # Y-M-D H:M:S Calibration for One-Point Cal (50.0 mS/cm)
endLow1<-'2021-01-17 08:44:00'

# Date of post-deployment calibrations
startLow2<-'2021-01-19 13:54:00' # Y-M-D H:M:S Calibration for One-Point Cal (50.0 mS/cm)
endLow2<-'2021-01-19 13:59:00'

# common garden
CGstart<-'2021-01-17 10:02:00'
CGend<-'2021-01-17 10:04:00'
# common garden CT average
CG_CT<-34.711 # 1-17-21
#CG_CT<-36.544 # 1-18-21

# Date of in situ logs
Launch<-'2021-01-17 12:30:00'
Retrieval<-'2021-01-19 09:30:00'

###################################
# Conductivity Calibration Standards and Logging Interval
###################################

# Two-Point Calibration Standards
refLow<-1413 # uS/cm ; ThermoScientific Orion Application Solution: 1413 uS/cm at 25degC Conductivity Standard
refHigh<-50000 # uS/cm ; ThermoScientific Orion Application Solution: 12.9 mS/cm at 25degC Conductivity Standard

###################################
# Pressure data
###################################
# COMMENT OUT ONE OF THE FOLLOWING

### If pairing with Pressure/Depth Logger data
Serial.depth<-'872' # Serial number of paired hobo pressure logger

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
path.Log<-paste0('Data/Cond_temp/Raw_csv')
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
startHigh1<-startHigh1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endHigh1<-endHigh1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
startHigh2<-startHigh2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endHigh2<-endHigh2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
startLow1<-startLow1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endLow1<-endLow1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
startLow2<-startLow2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endLow2<-endLow2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
CGstart<-CGstart%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
CGend<-CGend%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

# Filter out dates
lowCal<-condCal%>%filter(between(date,startLow1,endLow1)|between(date,startLow2,endLow2))
highCal<-condCal%>%filter(between(date,startHigh1,endHigh1)|between(date,startHigh2,endHigh2))
condLog<-condLog%>%filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval)) 
condCal<-union(lowCal,highCal) # Join calibration files together
CT.data<-union(condCal,condLog)# Join Calibration and Logged files
#CT.data$date<-CT.data$date - seconds(5) # offset time if necessary

############################################################
# Load Pressure Data from HOBO Pressure Loggers for In Situ Practical Salinity Calculations

if(exists('Serial.depth')) {
  path.depth<-'Data/Depth/Calibrated_files'
  file.names.depth<-basename(list.files(path.depth,pattern = c(Serial.depth, file.date,"csv$"), recursive = F))
  Depth.data <- file.names.depth %>%
    purrr::map_dfr(~ read_csv(file.path(path.depth, .),col_names=TRUE))
  Depth.data<-Depth.data%>%
    dplyr::filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval))%>%
    dplyr::rename(Serial.depth=Serial,TempInSitu.depth=TempInSitu)%>%
    dplyr::mutate(AbsPressure_bar=AbsPressure*0.01) # convert kPa to Bar (conversion: 1 kPa = 0.01 Bar)
} else {
  Depth.data<-tibble::tibble(date=CT.data$date, AbsPressure_bar=Pres_bar)
}
CT.data<-CT.data%>% # amend to larger dataframe
  left_join(Depth.data,by='date')

############################################################
# Two Point Calibration 

# mean E_Conductivity measured by probes in calibration solutions
rawLow<-CT.data%>%
  filter(between(date,startLow2,endLow2))%>%
  summarise(mean(E_Conductivity))%>%
  as.numeric()
rawHigh<-CT.data%>%
  filter(between(date,startHigh2,endHigh2))%>%
  summarise(mean(E_Conductivity))%>%
  as.numeric()

# mean TempInSitu measured by probes in calibration solutions
lowTemp<-CT.data%>%
  filter(between(date,startLow2,endLow2))%>%
  summarise(mean(TempInSitu))%>%
  as.numeric()
highTemp<-CT.data%>%
  filter(between(date,startHigh2,endHigh2))%>%
  summarise(mean(TempInSitu))%>%
  as.numeric()

# Adjusted E_Conductivity of calibration solutions to logger-recorded temperature
refLow<-29.945*mean(lowTemp) + 664.28 # for low conductivity of 1413 uS/cm
refHigh<-1060*mean(highTemp) + 23500 # for high conductivity of 50,000 uS/cm

rawRange<-rawHigh - rawLow
refRange<-refHigh - refLow

CT.data<-CT.data %>%
  mutate(E_Conductivity_cal = (((E_Conductivity - rawLow) * refRange) / rawRange) + refLow)

############################################################
# Correct for drift between pre- and post-deployment calibrations

# mean logger readings when in high conductivity solution at starting and ending calibration times
meanHigh1<-CT.data%>%
  filter(between(date,startHigh1,endHigh1))%>%
  summarise(mean(E_Conductivity_cal))%>%
  as.numeric()
meanHigh2<-CT.data%>%
  filter(between(date,startHigh2,endHigh2))%>%
  summarise(mean(E_Conductivity_cal))%>%
  as.numeric()

# mean logger readings when in low conductivity solution at starting and ending calibration times
meanLow1<-CT.data%>%
  filter(between(date,startLow1,endLow1))%>%
  summarise(mean(E_Conductivity_cal))%>%
  as.numeric()
meanLow2<-CT.data%>%
  filter(between(date,startLow2,endLow2))%>%
  summarise(mean(E_Conductivity_cal))%>%
  as.numeric()

# calculate max drift from differences between starting and ending conductivity readings
drift.off.High<-meanHigh1-meanHigh2
drift.off.Low<-meanLow1-meanLow2
drift.off<-max(c(drift.off.High,drift.off.Low))
drift.corr=drift.off/length(condLog$date)

# fill in absolute pressure at common garden (near surface ~= 0)
# used more for finding differences in logger readings than absolute values
CGpres<-CT.data%>%
  filter(between(date,CGstart,CGend))%>%
  mutate(AbsPressure_bar=0)

# adjust conductivity readings for drift (calculated above)
condLog<-CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  union(CGpres)%>%
  arrange(date)%>%
  mutate(drift.correction.new=drift.corr)%>% # establish a column filled with the drift correction value
  mutate(drift.correction=cumsum(drift.correction.new))%>% # fill the drift correction column with sequentially larger drift corrections from correlation value to full drift
  select(-drift.correction.new)%>%
  mutate(E_Conductivity_cal.drift = E_Conductivity_cal + drift.correction)
condCal1b<-CT.data%>%
  filter(between(date,startHigh1,endHigh1)|between(date,startLow1,endLow1))%>%
  mutate(E_Conductivity_cal.drift = E_Conductivity_cal)
condCal2b<-CT.data%>%
  filter(between(date,startHigh2,endHigh2)|between(date,startLow2,endLow2))%>%
  mutate(E_Conductivity_cal.drift = E_Conductivity_cal + drift.off)
condCal<-union(condCal1b,condCal2b)%>% # Join calibration files together
  mutate(AbsPressure_bar = 0) 
CT.data<-full_join(condCal,condLog) # Join Calibration and Logged files together

############################################################
# Calculate Salinity using gsw package for the PSS-78 equation

CT.data<-CT.data%>%
  mutate(SalinityInSitu_2pCal=gsw_SP_from_C(C = E_Conductivity_cal*0.001, t = TempInSitu, p=AbsPressure_bar)) # Use PSS-78 Equations for Salinity calculation

############################################################
# Adjust salinity readings based on common garden averages
CG_logged<-CT.data%>%
  filter(between(date,CGstart,CGend))%>%
  summarise(mean(SalinityInSitu_2pCal))%>%
  as.numeric()
CG_offset <-CG_CT - CG_logged
CT.data<-CT.data%>%
  mutate(SalinityInSitu_2pCal_wOffset = SalinityInSitu_2pCal + CG_offset)

############################################################
# Graph data

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu,color=SalinityInSitu_2pCal))+
  geom_line()+
  scale_colour_gradient(high = "#132B43",low = "#56B1F7")

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_2pCal_wOffset>20)%>%
  ggplot(aes(x=date,y=SalinityInSitu_2pCal_wOffset,color=TempInSitu))+
  geom_line()

# CT.data%>%
#   filter(between(date,Launch,Retrieval))%>%
#   ggplot(aes(x=date,y=-Depth,color=SalinityInSitu_1pCal))+
#   geom_line()+
#   scale_colour_gradient(high = "#132B43",low = "#56B1F7")

# CT.data%>%
#   filter(between(date,Launch,Retrieval))%>%
#   ggplot(aes(x=date,y=-Depth,color=TempInSitu))+
#   geom_line()

# Save salinity profile graph, colored by temperature
# CT.data%>%
#   filter(between(date,Launch,Retrieval))%>%
#   filter(SalinityInSitu_1pCal>24)%>%
#   ggplot(aes(x=date,y=SalinityInSitu_1pCal,color=TempInSitu))+
#   geom_line()+
#   ggsave(paste0('Output/CT_Cal/new_2pCal/CT_',serial,'_',file.date,'_0118-2021.png'),height = 10,width = 13)

# Write CSV file
write_csv(CT.data,paste0('Data/Cond_temp/Calibrated_files/',file.date,'_CT',serial,'_insitu_2pcal_withCGcorr.csv'))
 
