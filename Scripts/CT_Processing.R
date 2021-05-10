##########################################################################
##########################################################################
#### Conductivity Calibration script for HOBO Conductivity-Temperature logger data
#### Brings in raw .csv files by Serial number and exports a tidy file

#### Reference: https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf
#### Reference: https://www.aqion.de/site/112

# Author: Danielle Barnas
# created: 9-23-2020
# modified: 3-25-2021

##########################################################################
##########################################################################

### Brings in calibration start and stop times and logger start and stop times in one large csv log
### Avoids need to input a particular serial number unless otherwise desired

###################################
### Load Libraries
###################################

#devtools::install_github("dbarnas/mooreasgd") # if package has updated since last run
library(tidyverse)
library(lubridate)
library(gsw)
library(here)
library(gridExtra)
library(mooreasgd)

rm(list=ls())


###################################
### File Paths
###################################

### Input
# Path to folder storing logger .csv files
# path.cal<-here("Final_Project","Data","Cond_temp") # Logger calibration file path, if different from log path
path.log<-here("Data","Cond_temp") # Logger in situ file path (CT and Water Level files)
file.date <- "011721" # logger date used in file name(s)

### Output
# Path to store logger files
path.output<-here("Output","QC","Spatial","011721") # Output file path

###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd('2021-01-17')
end.date <- ymd('2021-01-17')

###################################
### Import calibration and launch records
###################################

# Read in files that are updated with calibration and launch information
calibration.log<-read_csv(here("Data","CT_Calibration_Log.csv")) # Calibration time logs
launch.log<-read_csv(here("Data","Launch_Log.csv")) %>%  # Launch time logs
  filter(Log_Type == "CT")

###################################
### Conductivity Calibration Standards and Logging Interval
###################################

# Two-Point Calibration Standard
highCal<-50000 # uS/cm at 25deg C
lowCal<-1413 # uS/cm at 25deg C

###################################
### Pressure data
###################################

### Pressure at Mo'orea sites changes salinity nominally, so using consistent Absolute Pressure (dbar) for Salinity calculation
Pres_dbar<-10 # surface pressure in decibar


#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

############################################################
### Read in Calibration and Logger Files
### Temperature compensation (source: https://www.aqion.de/site/112)
############################################################

# roundup function pulled from 'mooreasgd' package
# Reads in all raw csv's and returns multiple tidied csv's
# cleanup function pulled from 'mooreasgd' package
# Reads in raw csv and returns tidied csv for the probe with the specified serial number

# Conductivity Calibration files, if different file path from in situ logs
# condCal<-CT_roundup(data.path = path.cal, ct.pattern = c(file.date,"csv$"), tf_write = F)
# condCal<-CT_cleanup(path = path.cal, ct_serial = Serial)

# In Situ Conductivity files
condLog<-CT_roundup(data.path = path.log, ct.pattern = c(file.date,"csv$"), tf_write = F)
# condLog<-CT_cleanup(path = path.log, ct_serial = Serial)

############################################################
### Parse date and time
############################################################

### Parse dates into date and type vector types

## Calibration
## unite date to time columns and parse to POSIXct datetime
calibration.log <- calibration.log %>% 
  unite(col = 'time.in.high', date,time.in.high, sep = " ", remove = F) %>% 
  unite(col = 'time.out.high', date,time.out.high, sep = " ", remove = F) %>% 
  unite(col = 'time.in.low', date,time.in.low, sep = " ", remove = F) %>% 
  unite(col = 'time.out.low', date,time.out.low, sep = " ", remove = F) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(time.in.high = mdy_hms(time.in.high)) %>% 
  mutate(time.out.high = mdy_hms(time.out.high)) %>% 
  mutate(time.in.low = mdy_hms(time.in.low)) %>% 
  mutate(time.out.low = mdy_hms(time.out.low)) %>% 
  filter(date == start.date | date == end.date) %>%  # filter only current pre- and post-calibration dates
  select(-date) # remove date column
  
## in situ
## unite date to time columns and parse to POSIXct datetime
launch.log <- launch.log %>% 
  unite(col = 'start', date,start, sep = " ", remove = F) %>% 
  unite(col = 'end', date,end, sep = " ", remove = F) %>% 
  unite(col = 'cg.start', date,cg.start, sep = " ", remove = F) %>% 
  unite(col = 'cg.end', date,cg.end, sep = " ", remove = F) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(start = mdy_hms(start)) %>% 
  mutate(end = mdy_hms(end)) %>% 
  mutate(cg.start = mdy_hms(cg.start)) %>% 
  mutate(cg.end = mdy_hms(cg.end))%>% 
  filter(date == start.date | date == end.date) %>%  # filter only current launch dates
  select(-date) # remove date column



############################################################
### Calibration
############################################################

# get length of vector with all serial numbers for date(s) specified
n1 <- calibration.log %>%
  distinct(Serial) %>%
  nrow() %>%
  as.numeric()

# create empty df to add date-filtered data
CalLog<-tibble(date = as.POSIXct(NA),
            Serial = as.character(),
            TempInSitu = as.numeric(),
            E_Conductivity = as.numeric(),
            EC_Cal.1 = as.numeric(),
            EC_Cal.2 = as.numeric(),
            Salinity = as.numeric())
  
# Filter out calibration date and time and return dataframe with all logger calibration logs
for(i in 1:n1) {
  
  sn<-calibration.log %>% # vector of all serial numbers
    distinct(Serial) %>% # pull each distinct Serial number
    separate(col = 'Serial', into = c(NA,'Serial'), sep = "_")
  sn<-as.character(sn[i,]) # i'th serial number
  
  # filter by the i'th serial number
  C1<-condLog %>% 
    filter(Serial == stringr::str_subset(condLog$Serial, pattern = sn))
  C2<-calibration.log %>% 
    filter(Serial == stringr::str_subset(calibration.log$Serial, pattern = sn))
  
  # pull out calibration times
  startHigh <- C2$time.in.high
  endHigh <- C2$time.out.high
  startLow <- C2$time.in.low
  endLow <- C2$time.out.low
  
  # single time point calibration
  if(nrow(C2) == 1){ 
    
    # if low standard calibration = TRUE, then two-point calibration
    if(is.na(C2$time.in.low[1]) == F){ 
      calibration<-CT_two_cal(data = C1, high.Ref = highCal, low.Ref = lowCal, startHigh = startHigh, 
                 endHigh = endHigh, startLow = startLow, endLow = endLow,
                 date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity) %>% 
        rename(EC_Cal.1 = EC_Cal) %>% 
        mutate(EC_Cal.2 = NA)
      
      } else {
    
      # if low standard calibration = FALSE, then one-point calibration
      calibration<-CT_one_cal(data = C1, cal.ref = highCal, startCal = startHigh, endCal = endHigh, 
                              date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity) %>% 
        rename(EC_Cal.1 = EC_Cal) %>% 
        mutate(EC_Cal.2 = NA)
      
      }
    
    }
  
  # double time point calibration, pre- and post-deployment
  if(nrow(C2) == 2){ 
    
    # if low standard calibration = TRUE, then two-point calibration
    if(is.na(C2$time.in.low[1]) == F){ 
      preCal<-CT_two_cal(data = C1, date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity,
                         high.Ref = highCal, low.Ref = lowCal, 
                         startHigh = startHigh[1], endHigh = endHigh[1], 
                         startLow = startLow[1], endLow = endLow[1]) %>% 
        rename(EC_Cal.1 = EC_Cal)
      
      postCal<-CT_two_cal(data = C1, date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity,
                          high.Ref = highCal, low.Ref = lowCal, 
                          startHigh = startHigh[2], endHigh = endHigh[2], 
                          startLow = startLow[2], endLow = endLow[2]) %>% 
        rename(EC_Cal.2 = EC_Cal)
      
      calibration <- preCal %>% 
        full_join(postCal)
    } else {
      
      # if low standard calibration = FALSE, then one-point calibration
      preCal<-CT_one_cal(data = C1, cal.ref = highCal, date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity,
                         startCal = startHigh[1], endCal = endHigh[1]) %>% 
        rename(EC_Cal.1 = EC_Cal)
      
      postCal<-CT_one_cal(data = C1, cal.ref = highCal, date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity,
                          startCal = startHigh[2], endCal = endHigh[2]) %>% 
        rename(EC_Cal.2 = EC_Cal)
      
      calibration <- preCal %>% 
        full_join(postCal) 
    }

  }
  
  # Calculate Practical Salinity using gsw package with PSS-78 equation
  calibration <- calibration %>% 
    mutate(Salinity = gsw_SP_from_C(C = EC_Cal.1*0.001, t = TempInSitu, p = Pres_dbar))
  
  
  CalLog <- CalLog %>% 
    rbind(calibration) # add i'th logger's data to running dataframe
}


############################################################
### In Situ Logger Data
############################################################

n2 <- launch.log %>%
  distinct(Serial) %>%
  nrow() %>%
  as.numeric()

# create empty df to add date-filtered data
Log <-tibble(date = as.POSIXct(NA),
            Serial = as.character(),
            TempInSitu = as.numeric(),
            E_Conductivity = as.numeric(),
            EC_Cal.1 = as.numeric(),
            EC_Cal.2 = as.numeric(),
            Salinity = as.numeric())

# create list for storing plots
p <- list()

# Pull out in situ logger data
for(i in 1:n2) {
  
  sn <- launch.log %>% # vector of all serial numbers
    distinct(Serial) %>% 
    separate(col = 'Serial', into = c(NA,'Serial'), sep = "_")
  sn <- as.character(sn[i,]) # i'th serial number
  
  # filter by the i'th serial number
  C1<-CalLog %>% 
    filter(Serial == str_subset(CalLog$Serial, pattern = sn)) %>% 
    mutate(Serial = paste0("CT_",sn)) # make serial the same for easy join
  C2<-launch.log %>% 
    filter(Serial == str_subset(launch.log$Serial, pattern = sn))
  
  C1<-C1 %>% 
    filter(between(date, C2$cg.start[1], C2$cg.end[1]) |
             between(date, C2$start[1], C2$end[nrow(C2)]))
  
  # pull out original CT serial name
  sn <- CalLog %>% 
    filter(Serial == str_subset(CalLog$Serial, pattern = sn)) %>% # filter for i'th sn
    select(Serial)
  sn <- as.character(sn[1,]) # character string of full serial name
  
  C1 <- C1 %>% 
    mutate(Serial = sn)

  
  # Create Plot and save to list p
  p[[i]] <- C1 %>% 
    filter(Salinity > 25) %>% 
    ggplot(aes(x = date, y = Salinity, color = TempInSitu)) + 
    geom_point() + 
    theme_bw() +
    labs(x = "Date", color = "Temperature (C)") +
    ggtitle(sn)
  
  
  write.csv(C1, paste0(path.output,"/QC_",sn,".csv"))
  
  Log<-Log %>% 
    rbind(C1) %>% 
    distinct()
  
}

# Save all plots in a single dated pdf
pdf(paste0(path.output,"/",file.date,"_CT_plots.pdf"), onefile = TRUE)
for (i in seq(length(p))) {
  tplot <- p[[i]]
  print(tplot)
}
dev.off()

###########################################
# End of Script



###########################################
# Additional Information Below


# Saving plots
#
# if(plot.save == T)
#
# https://stackoverflow.com/questions/12234248/printing-multiple-ggplots-into-a-single-pdf-multiple-plots-per-page
# This solution is independent of whether the lengths of the lists in the list p are different.
# library(gridExtra)
# 
# pdf("plots.pdf", onefile = TRUE)
# for (i in seq(length(p))) {
#   do.call("grid.arrange", p[[i]])  
# }
# dev.off()

# Offset time if necessary
#CT.data$date<-CT.data$date - seconds(5) 




############################################################
### Apply drift calculation
### Only required for long deployments
############################################################

# # Logger data in pre-deployment calibration
# preCal<-CT.data%>%
#   filter(between(date,startCal1,endCal1))%>%
#   summarise(mean(Sp_Conductance))%>%
#   as.numeric
# # Logger data in post-deployment calibration
# postCal<-CT.data%>%
#   filter(between(date,startCal2,endCal2))%>%
#   summarise(mean(Sp_Conductance))%>%
#   as.numeric()

# # Drift between high calibration readings
# drift.off<-preCal-postCal
# # Drift correction factor
# drift.corr=drift.off/length(condLog$date)
# 
# condLog<-CT.data%>%
#   filter(between(date,CGstart,CGend)|between(date,Launch,Retrieval))%>%
#   arrange(date)%>%
#   mutate(drift.correction.new=drift.corr)%>% # establish a column filled with the drift correction value
#   mutate(drift.correction=cumsum(drift.correction.new))%>% # fill the drift correction column with sequentially larger drift corrections from correlation value to full drift
#   select(-drift.correction.new)%>%
#   mutate(Sp_Conductance.calDrift = Sp_Conductance_cal + drift.correction)
# condCal1b<-CT.data%>%
#   filter(between(date,startCal1,endCal1))%>%
#   mutate(Sp_Conductance.calDrift = Sp_Conductance_cal)
# condCal2b<-CT.data%>%
#   filter(between(date,startCal2,endCal2))%>%
#   mutate(Sp_Conductance.calDrift = Sp_Conductance_cal + drift.off)
# condCal<-union(condCal1b,condCal2b) # Join calibration files together
# CT.data<-full_join(condCal,condLog) # Join Calibration and Logged files



#### IGNORE BELOW - DO NOT RUN

# Nonlinear temperature compensation is calculated using the EC to SC equation from
# https://www.aqion.de/site/112


## INSTRUMENT DRIFT COMPENSATION 

## account for instrument drift over time
## reference: https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf

# each correction is based on linear drift over time
# data taken closer to initial calibration are corrected less than data taken toward the end of the monitoring period
# ft = correction factor
# t = the time interval for each data point that has passed since deployment
# total.t = total deployment time
# ft<-(t / total.t)

# one-point calibrations
# C = drift-corrected water quality parameter value
# m = uncorrected value
# si = value of the calibration standard
# sf = the value read by the instrument for the calibration standard after the total deployment time (total.t)
# C1<-m+ft*(si-sf)


############################################################
############################################################
# # One Point Calibration with Drift
# 
# # Drift correction factor
# 
# # each correction is based on linear drift over time
# # data taken closer to initial calibration are corrected less than data taken toward the end of the monitoring period
# t<-int #seconds  # t = the time interval for each data point that has passed since deployment
# total.t<-as.numeric(as.duration(Retrieval-Launch),"seconds") #seconds # total.t = total deployment time
# ft<-(t / total.t) # ft = correction factor
# 
# # one-point calibrations
# # C1<-m+ft*(si-sf)
# # C = drift-corrected water quality parameter value
# # Sp_Conductance = m = uncorrected value (mS/cm)
# si<-oneCal # the calibration standard
# sf<-CT.data%>% # the value read by the instrument in the calibration standard after the total deployment time (total.t)
#   filter(between(date,startCal1,endCal1))%>%
#   summarise(Middle=mean(Sp_Conductance)) # use avg calibration reading to avoid skew by placing logger in or taking logger out of calibration solution
# sf<-as.numeric(sf[1,])
# 

