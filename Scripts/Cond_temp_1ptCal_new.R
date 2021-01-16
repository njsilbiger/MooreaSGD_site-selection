# One-point Conductivity Calibrations for the HOBO CT Loggers using raw data off of HOBOware

# https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf

# created: 9-23-2020 by Danielle Barnas
# modified: 1-13-2021

#### Conductivity Calibration for Drift #####

rm(list=ls())

library(tidyverse)
library(lubridate)
library(gsw)
library(here)
library(driftR)

here()


###################################
# File Paths and Serial Numbers
###################################

cal.date<-'011521' # Date of logger calibration
file.date<-'011521' # Date in the logger file's name
Serial<-'318' # CT Probe Serial Number

###################################
# Date and Time
###################################
### Maintain date time format "YYYY-MM-DD HH:MM:SS"

# Date of initial calibrations
startCal1<-'2021-01-15 08:42:12' # Y-M-D H:M:S Calibration for One-Point Cal (50.0 mS/cm)
endCal1<-'2021-01-15 08:48:12'

# Date of in situ logs
Launch<-'2021-01-15 09:54:32'
Retrieval<-'2021-01-15 13:52:32'

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
Serial.depth<-'876' # Serial number of paired hobo pressure logger

### If data were recorded at a consistent pressure (bar)
#Pres_bar<-0

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
  map_dfr(~ read_csv(file.path(path.Cal, .),skip=1,col_names=TRUE))
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
  map_dfr(~ read_csv(file.path(path.Log, .),skip=1,col_names=TRUE))
condLog<-condLog%>% # Filter specified probe by Serial number
  select(contains('Date'),contains(Serial))%>%
  mutate(Serial=Serial)%>%
  rename(date=contains("Date"),TempInSitu=contains("Temp"),E_Conductivity=contains("High Range"))%>%
  drop_na()
condLog$date<-condLog$date%>%parse_datetime(format = "%m/%d/%y %H:%M:%S %p", na = character(), locale = default_locale(), trim_ws = TRUE) # Convert 'date' to date and time vector type

# Parse date filters into date and type vector types
startCal1<-startCal1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endCal1<-endCal1%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

# Filter out dates
condCal<-condCal%>%filter(between(date,startCal1,endCal1)) 
condLog<-condLog%>%filter(between(date,Launch,Retrieval)) 
CT.data<-union(condCal,condLog)# Join Calibration and Logged files

### 2. Calculate the non-linear temperature coefficient (alpha) [non-linear for groundwater influence]
theta<-mean(condCal$TempInSitu)
Cond_Ref<-1060*theta+23500

# Calculate f25 using equation K25 = f25 * K.theta
f25<-oneCal / mean(condCal$E_Conductivity)

#temperature correction factor, f25, for the conversion of conductivity vlues from theta to 25C
### Back calculate conductivity of sample series using f25 and equation above
CT.data<-CT.data%>%
  mutate(Sp_Conductance = f25 * E_Conductivity)


############################################################
############################################################
# Load Pressure Data from HOBO Pressure Loggers for In Situ Practical Salinity Calculations
If(Serial.depth = TRUE) {
  Depth.data<-read_csv(paste0('Data/Depth/',file.date,'_Depth',Serial.depth,'.csv'))
  Depth.data<-Depth.data%>%
    filter(between(date,Launch,Retrieval))%>%
    rename(Serial.depth=Serial,TempInSitu.depth=TempInSitu)%>%
    mutate(AbsPressure_bar=AbsPressure*0.01) # convert kPa to Bar (conversion: 1 kPa = 0.01 Bar)
} else {
  Depth.data<-tibble(date=CT.data$date, AbsPressure_bar=Pres_bar)
}
CT.data<-CT.data%>% # amend to larger dataframe
  left_join(Depth.data,by='date')

CT.data<-CT.data%>%
  separate(col=date,into=c("date","time"),sep=" ")
CT.data$date<-CT.data$date%>%parse_date(format = "%Y-%m-%d", na = character(),locale = default_locale(), trim_ws = TRUE)
CT.data$time<-CT.data$time%>%parse_time(format = "%H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

CT.data<-CT.data%>%
  dr_factor(corrFactor = Sp_Cond_drift.corr, dateVar = date, timeVar = time, keepDateTime = F)
#CT.data<-CT.data%>%
#  dr_correctOne(sourceVar = Sp_Conductance, cleanVar = Sp_Conductance.1pcal, calVal = Cond_Ref, calStd = oneCal, factorVar = Sp_Cond_drift.corr)

CT.data<-CT.data%>%
  mutate(Sp_Conductance.1pcal=Sp_Conductance + Sp_Cond_drift.corr)
CT.data<-CT.data%>%
  mutate(Sp_Cond_mS.cm=Sp_Conductance.1pcal*0.001)%>% # Sp_Conductance in mS/cm
  mutate(SalinityInSitu_1pCal=gsw_SP_from_C(C = Sp_Cond_mS.cm, t = TempInSitu, p=AbsPressure_bar))%>% # Use PSS-78 Equations for Salinity calculation
  unite(col=date,c('date','time'),sep=" ",remove=T)
CT.data$date<-CT.data$date%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

CT.data$date<-CT.data$date - seconds(6)

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  #filter(SalinityInSitu_1pCal>27)%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()

# Write CSV file and graph data
write_csv(CT.data,paste0('Data/Cond_temp/Calibrated_files/',file.date,'_CT',Serial,'_1pcal.csv'))

