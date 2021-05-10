##########################################################################
##########################################################################
#### Conductivity-Temperature-Depth Calibration script for HOBO Conductivity-Temperature and Water Level data
#### Brings in raw .csv files by Serial number and exports a tidy file

# Author: Danielle Barnas
# created: 5-01-2021
# modified: 5-10-2021

##########################################################################
##########################################################################


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
path.cal<-here("Data","May2021","Cond_temp") # Logger calibration file path
path.log<-here("Data","May2021","Cond_temp") # Logger in situ file path (CT and Water Level files)
path.WL<-here("Data","May2021","Pressure")
file.date <- "011821" # logger date used in file name(s)

### Output
# Path to store logger files
path.output<-here("Output","May2021","QC","Timeseries","011821") # Output file path


###################################
### Logger Serial Numbers
###################################

CT_Serial <- "324"
WL_Serial <- "877"

###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd('2021-01-18')
end.date <- ymd('2021-01-20')

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
highCal<-53000 # uS/cm at 25deg C
lowCal<-1413 # uS/cm at 25deg C



#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

############################################################
### Read in Calibration and Logger Files
### Temperature compensation (source: https://www.aqion.de/site/112)
############################################################

# cleanup function pulled from 'mooreasgd' package
# Reads in raw csv and returns tidied csv for the probe with the specified serial number

# In Situ Conductivity file
condLog <- CT_cleanup(data.path = path.log, ct.serial = CT_Serial) %>% 
  rename(CT_Serial = Serial)

# Conductivity Calibration files, if different from logs path
 #condCal<-CT_cleanup(path = path.cal, ct_serial = CT_Serial)

# In Situ Pressure/Depth file
presLog <- WL_cleanup(data.path = path.WL, wl.serial = WL_Serial) %>% 
  rename(WL_TempInSitu = TempInSitu,
         WL_Serial = Serial)


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

# Filter out calibration date and time 
  # filter calibration log by CT serial number
  C2<-calibration.log %>% 
    filter(Serial == stringr::str_subset(calibration.log$Serial, pattern = CT_Serial))
  
  # pull out calibration times
  startHigh <- C2$time.in.high
  endHigh <- C2$time.out.high
  startLow <- C2$time.in.low
  endLow <- C2$time.out.low
  
  # single time point calibration
  if(nrow(C2) == 1){ 
    
    # if low standard calibration = TRUE, then two-point calibration
    if(is.na(startLow[1]) == F){ 
      calibration<-CT_two_cal(data = condLog, high.Ref = highCal, low.Ref = lowCal, startHigh = startHigh, 
                              endHigh = endHigh, startLow = startLow, endLow = endLow,
                              date = condLog$date, temp = condLog$TempInSitu, EC = condLog$E_Conductivity) %>% 
        rename(EC_Cal.1 = EC_Cal) %>% 
        mutate(EC_Cal.2 = NA)
      
    } else {
      
      # if low standard calibration = FALSE, then one-point calibration
      calibration<-CT_one_cal(data = condLog, cal.ref = highCal, startCal = startHigh, endCal = endHigh, 
                              date = condLog$date, temp = condLog$TempInSitu, EC = condLog$E_Conductivity) %>% 
        rename(EC_Cal.1 = EC_Cal) %>% 
        mutate(EC_Cal.2 = NA)
      
    }
    
  } else { # if(nrow(C2) == 2){ # double time point calibration, pre- and post-deployment
    
    # if low standard calibration = TRUE, then two-point calibration
    if(is.na(startLow[1]) == F){ 
      preCal<-CT_two_cal(data = condLog, date = condLog$date, temp = condLog$TempInSitu, EC = condLog$E_Conductivity,
                         high.Ref = highCal, low.Ref = lowCal, 
                         startHigh = startHigh[1], endHigh = endHigh[1], 
                         startLow = startLow[1], endLow = endLow[1]) %>% 
        rename(EC_Cal.1 = EC_Cal)
      
      postCal<-CT_two_cal(data = condLog, date = condLog$date, temp = condLog$TempInSitu, EC = condLog$E_Conductivity,
                          high.Ref = highCal, low.Ref = lowCal, 
                          startHigh = startHigh[2], endHigh = endHigh[2], 
                          startLow = startLow[2], endLow = endLow[2]) %>% 
        rename(EC_Cal.2 = EC_Cal)
      
      calibration <- preCal %>% 
        full_join(postCal)
    } else {
      
      # if low standard calibration = FALSE, then one-point calibration
      preCal<-CT_one_cal(data = condLog, cal.ref = highCal, date = condLog$date, temp = condLog$TempInSitu, EC = condLog$E_Conductivity,
                         startCal = startHigh[1], endCal = endHigh[1]) %>% 
        rename(EC_Cal.1 = EC_Cal)
      
      postCal<-CT_one_cal(data = condLog, cal.ref = highCal, date = condLog$date, temp = condLog$TempInSitu, EC = condLog$E_Conductivity,
                          startCal = startHigh[2], endCal = endHigh[2]) %>% 
        rename(EC_Cal.2 = EC_Cal)
      
      calibration <- preCal %>% 
        full_join(postCal) 
    }
    
  }
  
  # Join Files by date
  CTD.data<-full_join(calibration, presLog)
  head(CTD.data)
  
  # Temperature Compensation (Linear relationship within temperature range observed through calibrations and in situ)
  ## Get linear model between temperature and alpha (temperature compensation coefficient)
  my.lm<-tibble(temp = c(31,30,29,28,27,26,25,24,23,22,21),
                 EC = c(55.99767,54.98681,53.98027,52.97817,51.98066,50.98789,50,49.01713,48.03943,47.06704,46.10012))
  my.lm<-my.lm %>% 
    mutate(alpha = (EC - 50)/(50*(temp - 25))*100) %>%  # alpha is the temperature compensation coefficient
    drop_na()
  model<-lm(alpha ~ temp, my.lm) # r2 = 0.9993
  b<-as.numeric(model$coefficients[1]) # intercept
  m<-as.numeric(model$coefficients[2]) # slope
  
  CTD.data <- CTD.data %>% 
    mutate(alpha = m * TempInSitu + b,
           SC_Cal.1 = EC_Cal.1 / (1 + (alpha/100) * (TempInSitu - 25)),
           SC_Cal.2 = EC_Cal.2 / (1 + (alpha/100) * (TempInSitu - 25)))
  
  
  # Resources:
  # https://in-situ.com/uk/pub/media/support/documents/Specific-Conductance-as-an-Output-Unit-for-Conductivity-Readings-Tech-Note.pdf
  # https://visaya.solutions/en/article/temperature-compensation-in-conductivity-measurement
  
  # Calculate Practical Salinity using gsw package with PSS-78 equation
  CTD.data <- CTD.data %>% 
    mutate(Salinity = gsw_SP_from_C(C = SC_Cal.2*0.001, t = 25, p = Pres_dbar))




############################################################
### In Situ Logger Data
############################################################


  # Filter out in situ readings date and time 
  C2<-launch.log %>% 
    filter(Serial == str_subset(launch.log$Serial, pattern = CT_Serial))
  
  CTD.data<-CTD.data %>% 
    filter(between(date, C2$cg.start[1], C2$cg.end[1]) |
             between(date, C2$start[1], C2$end[nrow(C2)]))
  
  # Plot CTD data
  p<-list()
  p[[1]]<-CTD.data %>% 
    filter(Salinity > 28) %>% 
    ggplot(aes(x = date, y = Salinity, color = TempInSitu)) + 
    geom_point() + 
    theme_bw() +
    labs(x = "Date", color = "Temperature (C)", y = "Salinity (psu)") +
    ggtitle(paste("CT",CT_Serial,"Salinity", file.date))
  p[[2]]<-CTD.data %>% 
    filter(Salinity > 28) %>% 
    ggplot(aes(x = date, y = Depth, color = Salinity)) + 
    geom_point() + 
    scale_y_reverse() +
    theme_bw() +
    labs(x = "Date", color = "Salinity", y = "Depth (m)") +
    ggtitle(paste("WL",WL_Serial,"Depth", file.date))
  p[[3]]<-CTD.data %>% 
    ggplot(aes(x = date, y = Depth, color = TempInSitu)) + 
    geom_point() + 
    scale_y_reverse() +
    theme_bw() +
    labs(x = "Date", color = "Temperature (C)", y = "Depth (m)") +
    ggtitle(paste("WL",WL_Serial,"Depth", file.date))
  
  # Save all plots in a single dated pdf
  pdf(paste0(path.output,"/",file.date,"_CTD_plots.pdf"), onefile = TRUE)
  for (i in seq(length(p))) {
    tplot <- p[[i]]
    print(tplot)
  }
  dev.off()

  
  write.csv(CTD.data, paste0(path.output,"/QC_CTD_",file.date,"_CT_",CT_Serial,"_WL_",WL_Serial,".csv"))

  
