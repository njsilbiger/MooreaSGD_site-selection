

# created: 3-10-2020 by Danielle Barnas
# modified: 9-29-2020

### BEFORE RUNNING SCRIPT
# Load HoboWare .hobo datafile in HoboWare software program
# Select Series 1 and 2 for Measurements Abs Pres and Temp in kPa and degC, respectively
# Deselect all events
# Select the Barometric Compensation Assistant and click Proces...
# Choose Salt Water (1,025.000 kg/m3)
# Check to Use a Reference Water Level and enter 0.000 Meters
# Select a Reference Time when Logger was exactly at the water surface prior to or post-deployment
# Call Resultant Series Name: Water Level and click Create New Series
# Check that Series 3 for Water Level in meters is selected and click Plot
# Export file into your dated data folder (path below)

rm(list=ls())

library(tidyverse)
library(lubridate)
library(here)

here()

###################################
# File Paths and Serial Numbers
###################################

file.date<-'011721' # Date in the logger file's name
Serial<-'876'

###################################
# Date and Time
###################################
### Maintain date time format "YYYY-MM-DD HH:MM:SS"

# Date of in situ logs
Launch<-'2021-01-17 10:27:00'
Retrieval<-'2021-01-17 13:13:00'

#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

# Read in Conductivity Calibration files
path.p<-'Data/Depth'
file.names<-basename(list.files(path.p, pattern = c(file.date,"csv$"), recursive = F)) #list all csv file names in the folder and subfolders
Depth.data<-file.names %>%
  map_dfr(~ read_csv(file.path(path.p, .),skip=1,col_names=TRUE))

# Filter specified probe by Serial number
Depth.data<-Depth.data%>%
  select(contains('Date'),contains(Serial))%>%
  mutate(Serial=Serial)%>%
  rename(date=contains("Date"),TempInSitu=contains("Temp"),AbsPressure=contains("Abs Pres"),Depth=contains("Water Level"))%>%
  drop_na()
Depth.data$date<-Depth.data$date%>%parse_datetime(format = "%m/%d/%y %H:%M:%S %p", na = character(), locale = default_locale(), trim_ws = TRUE) # Convert 'date' to date and time vector type

# Filter data to only include deployment data
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Depth.data<-Depth.data%>%filter(between(date,Launch,Retrieval)) 

# Create simple csv file
write_csv(Depth.data,paste0('Data/Depth/Calibrated_files/',file.date,'_Depth',Serial,'.csv'))

# Plot the data
Depth.data %>% # this is the dataframe
  ggplot(aes(x= date, y= -Depth))+   #setup plot with x and y data
  geom_line()
Depth.data %>% # this is the dataframe
  ggplot(aes(x= date, y= TempInSitu))+   #setup plot with x and y data
  geom_line()

