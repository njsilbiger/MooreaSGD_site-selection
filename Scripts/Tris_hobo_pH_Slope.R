### Tris calibration for HOBO pH MX2501 Loggers

library(tidyverse)
library(seacarb)
library(broom)
library(here)
library(lubridate)
library(gsw)
library(gridExtra)
library(mooreasgd)





###################################

### Input
# Path to folder storing logger .csv files
path.log<-here("Data","March2022","Varari_Sled", "20220404","raw_files") # Logger in situ file path (CT and Water Level files)
file.date <- "20220404" # logger date used in file name(s)

### Output
# Path to store logger files
path.output<-here("Data","March2022","Varari_Sled", "20220404","QC_files") # Output file path


###################################
### Logger Serial Numbers
###################################

pH_Serial <- "196"

###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd('2022-04-04')
end.date <- ymd('2022-04-14')

# do you want to plot a graph?
plotgraph<-'no'

###################################
### Import calibration and launch records
###################################

# Read in files that are updated with calibration and launch information
#calibration.log<-read_csv(here("Data","Tris_Calibration.csv")) # Calibration time logs
launch.log<-read_csv(here("Data","Launch_Log.csv")) %>%  # Launch time logs
  filter(Log_Type == "PH")


## bring in pH calibration files and raw data files
pHcalib<-read_csv(here("Data","March2022","CarbonateChemistry","TrisCalibrationLog.csv")) %>%
  mutate(TrisCalDate = mdy(TrisCalDate))



Salinity_lab<-34 # normally would be a column in pHData datasheet, from CT_Logger

#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

############################################################
### Read in Logger Files
############################################################

# cleanup function pulled from 'mooreasgd' package
# Reads in raw csv and returns tidied csv for the probe with the specified serial number

# In Situ pH file
pH.data <- pH_cleanup(data.path = path.log, pH.serial = pH_Serial) %>% 
  rename(pH_Serial = Serial)

############################################################
### Parse date and time
############################################################

### Parse dates into date and type vector types

launch.log <- launch.log %>%
  mutate(time_start = mdy_hm(time_start), # convert to time
         time_end = mdy_hm(time_end),
         start  = date(time_start), # extract the date
         end = date(time_end)) %>%
  filter(Serial == paste0("PH_",pH_Serial), # pulll out the right serial number
         start == ymd(start.date),
         end == ymd(end.date))

############################################################
### In Situ Logger Data
############################################################
pHLog<-pH.data %>% # extract the data you need
  filter(between(date,launch.log$time_start, launch.log$time_end))


# Convert 'date' to date and time vector type
# filter just the HOBO calibration
pHcalib<-pHcalib %>%
  filter(HOBO_Orion == "HOBO")

# 
# pHcalib$TrisCalDate<-pHcalib$TrisCalDate %>%
#   parse_datetime(format = "%m/%d/%y", na = character(),
#                  locale = default_locale(), trim_ws = TRUE)

## take the mV calibration files by each date and use them to calculate pH
pHSlope<-pHcalib %>%
  dplyr::nest_by(TrisCalDate) %>%
  dplyr::mutate(fitpH = list(lm(mVTris~TTris, data = data))) %>% # linear regression of mV and temp of the tris
  dplyr::summarise(broom::tidy(fitpH)) %>% # make the output tidy
  select(TrisCalDate, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%# put slope and intercept in their own column
#  full_join(pHData) %>% # join with the pH sample data
#  separate(date,sep=" ",into=c('date','time')) %>%
  bind_cols(pHLog)%>%
 # fill('(Intercept)',TTris) %>%
  drop_na() %>%
  mutate(mVTris = TempInSitu*TTris + `(Intercept)`) %>% # calculate the mV of the tris at temperature in which the pH of samples were measured
  mutate(pH_total = pH(Ex=mV,Etris=mVTris,S=Salinity_lab,T=TempInSitu)) %>% # calculate pH of the samples using the pH seacarb function
  #mutate(pH_insitu = pHinsi(pH = pH, ALK = TA_Raw, Tinsi = TempInSitu, Tlab = Temp, S = Salinity_lab_Silbiger)) %>%
  select(date,TempInSitu,pH_NBS = pH,mV,pH_total,mVTris) 

View(pHSlope)



if(plotgraph=='yes'){
  # Plot pH data
  p<-list()
  p[[1]]<-pHSlope %>% 
    ggplot(aes(x = date, y = pH_total, color = TempInSitu)) + 
    geom_line() + 
    theme_bw() +
    labs(x = "Date", color = "Temperature (C)") +
    ggtitle(paste("pH",pH_Serial))
}
  
## write the data
# write out the clean data
write_csv(pHSlope, paste0(path.output,"/QC_pH_",file.date,"_pHtot_",pH_Serial,".csv"))


