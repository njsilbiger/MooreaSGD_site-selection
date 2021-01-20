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

here()


###################################
# File Paths and Serial Numbers
###################################

cal.date<-'0117-1921' # Date of logger calibration
file.date<-'0117-1921' # Date in the logger file's name
Serial<-'316' # CT Probe Serial Number

###################################
# Date and Time
###################################
### Maintain date time format "YYYY-MM-DD HH:MM:SS"

# Date of pre-deployment calibrations
startCal1<-'2021-01-17 08:18:00' # Y-M-D H:M:S Calibration for One-Point Cal (50.0 mS/cm)
endCal1<-'2021-01-17 08:23:00'

# Date of post-deployment calibrations
startCal2<-'2021-01-19 13:38:00' # Y-M-D H:M:S Calibration for One-Point Cal (50.0 mS/cm)
endCal2<-'2021-01-19 13:46:00'

# Date of in situ logs
Launch<-'2021-01-17 11:14:00'
Retrieval<-'2021-01-19 10:08:00'

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
startCal2<-startCal2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
endCal2<-endCal2%>%parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

# Filter out dates
condCal1<-condCal%>%filter(between(date,startCal1,endCal1))
condCal2<-condCal%>%filter(between(date,startCal2,endCal2))
condLog<-condLog%>%filter(between(date,Launch,Retrieval)) 
condCal<-union(condCal1,condCal2) # Join calibration files together
CT.data<-union(condCal,condLog)# Join Calibration and Logged files
#CT.data$date<-CT.data$date - seconds(5) # offset time if necessary

############################################################
## Correct for drift between pre- and post-deployment calibrations

meanCalCond1<-mean(condCal1$E_Conductivity)
meanCalCond2<-mean(condCal2$E_Conductivity)
offsetCal<-meanCalCond1-meanCalCond2
drift.corr=offsetCal/length(condLog$E_Conductivity)

condLog<-condLog%>%
  mutate(drift.correction.new=drift.corr)%>%
  mutate(drift.correction=cumsum(drift.correction.new))%>%
  select(-drift.correction.new)
condLog<-condLog%>%
  mutate(E_Conductivity.corrected = E_Conductivity + drift.correction)
condCal1$E_Conductivity.corrected<-condCal1$E_Conductivity
condCal2$E_Conductivity.corrected<-condCal2$E_Conductivity + offsetCal
condCal<-union(condCal1,condCal2) # Join calibration files together
CT.data<-full_join(condCal,condLog)# Join Calibration and Logged files

############################################################
## Calculate temperature correction factor (f25)

# Calibration temperature and expected conductivity reading
theta<-mean(condCal1$TempInSitu) # mean logged temperature at pre-deployment
Cond_Ref<-1060*theta+23500 # actual calibration solution conductance at logged temperature at pre-deployment

# Calculate f25 correction factor using equation K25 = f25 * K.theta
f25<-oneCal / meanCalCond1

# Back calculate conductivity of sample series using f25 and equation above
CT.data<-CT.data%>%
  mutate(Sp_Conductance = f25 * E_Conductivity.corrected)

# linear compensation for conductivity > 10,000 uS/cm
#alpha<-((Cond_Ref - oneCal)*100) / (oneCal * (theta - 25)) # temperature coefficient of variation 
# Calculate Specific Conductance using the temperature coefficient alpha
#CT.data<-CT.data%>%
#  mutate(Sp_Conductance = E_Conductivity.corrected / (1 + (alpha / 100) * (TempInSitu - 25)))

############################################################
# Load Pressure Data from HOBO Pressure Loggers for In Situ Practical Salinity Calculations

If(Serial.depth = TRUE) {
  path.depth<-'Data/Depth/Calibrated_files'
  file.names.depth<-basename(list.files(path.depth,pattern = c(Serial.depth, file.date,"csv$"), recursive = F))
  Depth.data <- file.names.depth %>%
    map_dfr(~ read_csv(file.path(path.depth, .),col_names=TRUE))
  Depth.data<-Depth.data%>%
    filter(between(date,Launch,Retrieval))%>%
    rename(Serial.depth=Serial,TempInSitu.depth=TempInSitu)%>%
    mutate(AbsPressure_bar=AbsPressure*0.01) # convert kPa to Bar (conversion: 1 kPa = 0.01 Bar)
} else {
  Depth.data<-tibble(date=CT.data$date, AbsPressure_bar=Pres_bar)
}
CT.data<-CT.data%>% # amend to larger dataframe
  left_join(Depth.data,by='date')

############################################################
# Calculate Salinity using gsw package for the PSS-78 equation

CT.data<-CT.data%>%
  mutate(Sp_Cond_mS.cm=Sp_Conductance*0.001)%>% # Sp_Conductance in mS/cm
  mutate(SalinityInSitu_1pCal=gsw_SP_from_C(C = Sp_Cond_mS.cm, t = TempInSitu, p=AbsPressure_bar)) # Use PSS-78 Equations for Salinity calculation

############################################################
# Graph data

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()

CT.data%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()

# Write CSV file
write_csv(CT.data,paste0('Data/Cond_temp/Calibrated_files/fixed_cal/',file.date,'_CT',Serial,'_1pcal.csv'))
