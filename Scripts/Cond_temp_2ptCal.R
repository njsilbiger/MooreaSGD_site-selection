# Two-point Conductivity Calibrations for the HOBO CT Loggers using raw data off of HOBOware

# https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf

# created: 9-23-2020 by Danielle Barnas
# modified: 9-29-2020

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

cal.date<-'010921' # Date of logger calibration
file.date<-'011221' # Date in the logger file's name
Serial<-'316' # CT Probe Serial Number

###################################
# Pressure data
###################################

# COMMENT OUT ONE OF THE FOLLOWING

### If pairing with Pressure/Depth Logger data
#Serial.depth<-'872' # Serial number of paired hobo pressure logger

### If data were recorded at a consistent pressure (bar)
Pres_bar<-0

###################################
# Date and Time
###################################

### Maintain date time format "YYYY-MM-DD HH:MM:SS"

# Date of initial calibrations
startCal1<-'2021-01-09 16:59:00' # Y-M-D H:M:S Low Standard Calibration for Two-Point Cal (1413 uS/cm)
endCal1<-'2021-01-09 17:05:00'

startCal2<-'2021-01-09 17:18:20' # Y-M-D H:M:S High Standard Calibration for Two-Point Cal (50.0 mS/cm)
endCal2<-'2021-01-09 17:25:00'

# Date of in situ logs, Y-M-D, H:M:S
Launch<-'2021-01-12 10:00:00'
Retrieval<-'2021-01-12 14:20:00'

###################################
# Conductivity Calibration Standards and Logging Interval
###################################

# Two-Point Calibration Standards
lowRef<-1413 # uS/cm ; ThermoScientific Orion Application Solution: 1413 uS/cm at 25degC Conductivity Standard
highRef<-50000 # uS/cm ; ThermoScientific Orion Application Solution: 12.9 mS/cm at 25degC Conductivity Standard

#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

############################################################
############################################################
### Read in and Calibration and Logger Files

# Conductivity Calibration files
path.Cal<-'Data/Cond_temp/Calibration'
file.names.Cal<-basename(list.files(path.Cal, pattern = c(cal.date,"csv$"), recursive = F)) #list all csv file names in the folder and subfolders
condCal <- file.names.Cal %>%
  map_dfr(~ read_csv(file.path(path.Cal, .),skip=1,col_names=TRUE)) #,col_types=list("Button Down"=col_skip(),"Button Up"=col_skip(),"Host Connect"=col_skip(),"Stopped"=col_skip(),"EOF"=col_skip())))
condCal<-condCal%>% # Filter specified probe by Serial number
  select(contains('Date'),contains(Serial))%>%
  mutate(Serial=Serial)%>%
  rename(date=contains("Date"),TempInSitu=contains("Temp"),E_Conductivity=contains("High Range"))%>%
  drop_na()
condCal$date<-condCal$date%>%parse_datetime(format = "%m/%d/%y %H:%M:%S %p", na = character(), locale = default_locale(), trim_ws = TRUE) # Convert 'date' to date and time vector type

# In Situ Conductivity files
path.Log<-paste0('Data/Cond_temp')
file.names.Log<-basename(list.files(path.Log, pattern = c(file.date,"csv$"), recursive = F)) #list all csv file names in the folder and subfolders
condLog <- file.names.Log %>%
  map_dfr(~ read_csv(file.path(path.Log, .),skip=1,col_names=TRUE)) #,col_types=list("Button Down"=col_skip(),"Button Up"=col_skip(),"Host Connect"=col_skip(),"Stopped"=col_skip(),"EOF"=col_skip())))
condLog<-condLog%>% # Filter specified probe by Serial number
  select(contains('Date'),contains(Serial))%>%
  mutate(Serial=Serial)%>%
  rename(date=contains("Date"),TempInSitu=contains("Temp"),E_Conductivity=contains("High Range"))%>%
  drop_na()
condLog$date<-condLog$date%>%parse_datetime(format = "%m/%d/%y %H:%M:%S %p", na = character(), locale = default_locale(), trim_ws = TRUE) # Convert 'date' to date and time vector type

# Parse date filters into date and type vector types
startCal1<-startCal1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endCal1<-endCal1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
startCal2<-startCal2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endCal2<-endCal2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

# Filter out dates and Join Calibration and Logged files
lowCal<-condCal%>%filter(between(date,startCal1,endCal1))
highCal<-condCal%>%filter(between(date,startCal2,endCal2)) 
condLog<-condLog%>%filter(between(date,Launch,Retrieval)) 
condCal<-union(lowCal,highCal)
CT.data<-union(condCal,condLog)

############################################################
############################################################
# Load Pressure Data from HOBO Pressure Loggers for In Situ Practical Salinity Calculations
If(Serial.depth = TRUE) {
  data.pres<-read_csv(paste0('Probe_and_Logger_Protocols/HOBO_Pressure_Loggers/Data/',folder.date,'/',Serial.depth,'_HOBOdepth.csv'))
  data.pres<-data.pres%>%
    filter(between(date,Launch,Retrieval))%>%
    rename(Serial.depth=Serial,TempInSitu.depth=TempInSitu)%>%
    mutate(AbsPressure_bar=AbsPressure*0.01) # convert kPa to Bar (conversion: 1 kPa = 0.01 Bar)
} else {
  data.pres<-tibble(date=CT.data$date, AbsPressure_bar=Pres_bar)
}
CT.data<-CT.data%>% # ammend to larger dataframe
  left_join(data.pres,by='date')

############################################################
############################################################
# Two Point Calibration 

meanLowCal<-mean(lowCal$E_Conductivity)

CT.data<-CT.data %>%
  mutate(Sp_Cond_mS.cm =(((E_Conductivity-meanLowCal)*(highRef-lowRef))/(max(E_Conductivity)-min(E_Conductivity))+lowRef)*0.001) %>%
  mutate(SalinityInSitu_2pCal=gsw_SP_from_C(Sp_Cond_mS.cm, TempInSitu, p=AbsPressure_bar))

############################################################
############################################################
# Write CSV file and graph data
write_csv(CT.data,paste0('Data/Cond_temp/Calibrated_files/',file.date,'_CT',Serial,'_2pcal.csv'))
View(CT.data)

CT.data%>% # show low salinity points to either remove outliers or observe possible sgd points
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_2pCal<24)

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_2pCal>24)%>% # to ignore outliers
  ggplot(aes(x=date,y=SalinityInSitu_2pCal))+
  geom_line()
