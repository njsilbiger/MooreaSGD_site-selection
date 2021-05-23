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
path.log<-here("Data","May2021","Cond_temp","Csv_files","Calibrated_csv","051621_Varari_Benthic_Array") # Logger in situ file path (CT and Water Level files)
#path.WL<-here("Data","May2021","Depth")
file.date <- "051621" # logger date used in file name(s)


### Output
# Path to store logger files
path.output<-here("Data","May2021","Cond_temp","Csv_files","QC","Spatial_Array","Csv_files") # Output file path


###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd('2021-05-16')
end.date <- ymd('2021-05-18')


###################################
### Import calibration and launch records
###################################

# Read in files that are updated with calibration and launch information
calibration.log<-read_csv(here("Data","CT_Calibration_Log.csv")) # Calibration time logs
launch.log<-read_csv(here("Data","Launch_Log.csv")) %>%  # Launch time logs
  filter(Log_Type == "CT")


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
  unite(col = 'time_in', date,time_in, sep = " ", remove = F) %>% 
  unite(col = 'time_out', date,time_out, sep = " ", remove = F) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(time_in = mdy_hms(time_in)) %>% 
  mutate(time_out = mdy_hms(time_out)) %>% 
  filter(date == start.date | date == end.date) # filter only current pre- and post-calibration dates
  
## in situ
## unite date to time columns and parse to POSIXct datetime
launch.log <- launch.log %>% 
  separate(col = time_start, into = c("date_start","time_start"), sep = " ", remove = F) %>% # put start date in another column to filter by date
  separate(col = time_end, into = c("date_end","time_end"), sep = " ", remove = F) %>% # put end date in another column to filter by date
  mutate(date_start = mdy(date_start), # parse to date-time format
         date_end = mdy(date_end)) %>% 
  filter(date_start == start.date | date_end == end.date) %>% # filter only current launch dates
  unite(col = "time_start",date_start,time_start, sep = " ", remove = F) %>% # reunite date and time columns
  unite(col = "time_end",date_end,time_end, sep = " ", remove = F) 


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
  startHigh <- C2 %>%
    filter(cond_HL == 'H') %>% 
    select(time_in)
  endHigh <- C2 %>%
    filter(cond_HL == 'H') %>% 
    select(time_out)
  startLow <- C2 %>%
    filter(cond_HL == 'L') %>% 
    select(time_in)
  endLow <- C2 %>%
    filter(cond_HL == 'L') %>% 
    select(time_out)
  
  
  if(nrow(C2) == 1){ 
    
    # specify whether calibration reference is an electrical conductivity or specific conductance value
    if(C2$EC_SC == 'EC'){
      ec.cal = TRUE
    } else {ec.cal = FALSE}
    
    # single time point calibration
    calibration<-CT_one_cal(data = C1, cal.ref = C2$cond_uS[1], cal.ref.temp = C2$temp_C[1], startCal = startHigh, endCal = endHigh, 
                            date = C1$date, temp = C1$TempInSitu, EC.logger = C1$E_Conductivity, EC.cal = ec.cal) %>% 
      rename(EC_Cal.1 = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
             TempInSitu_logger = TempInSitu, # rename logger's temperature readings column
             TempInSitu = TempInSitu_Cal) %>%  # rename calibrated temperature readings, calibrated to secondary probe, if available
      mutate(EC_Cal.2 = NA)
    
    # TRUE if single calibration time
    singleCal = TRUE
  } else if(nrow(C2) == 2){
  
      # both High and Low calibration solutions present: two-point calibration
      if(nrow(distinct(C2,cond_HL)) == 2){ 
        
        # specify whether calibration reference is an electrical conductivity or specific conductance value
        if(C2$EC_SC[1] == 'EC'){ # assuming if one point is EC, the second is also EC
          ec.cal = TRUE
        } else {ec.cal = FALSE}  
        
        high.ref<-C2 %>% 
          filter(cond_HL == 'H') %>% 
          select(cond_uS)
        low.ref<-C2 %>% 
          filter(cond_HL == 'L') %>% 
          select(cond_uS)
        high.ref.temp<-C2 %>% 
          filter(cond_HL == 'H') %>% 
          select(temp_C)
        low.ref.temp<-C2 %>% 
          filter(cond_HL == 'L') %>% 
          select(temp_C)
        
        calibration<-CT_two_cal(data = C1, high.ref = high.ref[1], low.ref = low.ref[1], high.ref.temp = high.ref.temp[1], low.ref.temp = low.ref.temp[1],
                                startHigh = startHigh, endHigh = endHigh, startLow = startLow, endLow = endLow,
                                date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity, EC_probe = ec.cal) %>% 
          rename(EC_Cal.1 = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
                 TempInSitu_logger = TempInSitu, # rename logger's temperature readings column
                 TempInSitu = TempInSitu_Cal) %>%  # rename calibrated temperature readings, calibrated to secondary probe, if available
          mutate(EC_Cal.2 = NA)
      
          # TRUE if single calibration time
          singleCal = TRUE
      } else if(nrow(distinct(C2,cond_HL)) == 1){
        
        # only High or Low calibration solutions present: double calibration times at single calibration reference each time
        
        ## PRECAL
        # specify whether calibration reference is an electrical conductivity or specific conductance value
        if(C2$EC_SC[1] == 'EC'){
          ec.cal = TRUE
        } else {ec.cal = FALSE}
        
        preCal<-CT_one_cal(data = C1, cal.ref = C2$cond_uS[1], cal.ref.temp = C2$temp_C[1], date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity,
                           startCal = startHigh[1], endCal = endHigh[1], EC_probe = ec.cal) %>% 
          rename(EC_Cal.1 = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
                 TempInSitu_logger = TempInSitu, # rename logger's temperature readings column
                 TempInSitu = TempInSitu_Cal)  # rename calibrated temperature readings, calibrated to secondary probe, if available
          
        
        ## POSTCAL
        # specify whether calibration reference is an electrical conductivity or specific conductance value
        if(C2$EC_SC[2] == 'EC'){
          ec.cal = TRUE
        } else {
          ec.cal = FALSE}
        
        postCal<-CT_one_cal(data = C1, cal.ref = C2$cond_uS[2], cal.ref.temp = C2$temp_C[2], date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity,
                            startCal = startHigh[2], endCal = endHigh[2], EC_probe = ec.cal) %>% 
          rename(EC_Cal.2 = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
                 TempInSitu_logger = TempInSitu, # rename logger's temperature readings column
                 TempInSitu = TempInSitu_Cal)  # rename calibrated temperature readings, calibrated to secondary probe, if available
          
        
        calibration <- preCal %>% 
          full_join(postCal) 
        
        # FALSE if double calibration times
        singleCal = FALSE
      }
  } else if(nrow(C2) == 4){
    
    # both High and Low calibration solutions present: two-point calibration
    # double time points for High and Low calibration: pre- and post- deployment calibrations
    
    ## PRECAL
      
      # specify whether calibration reference is an electrical conductivity or specific conductance value
      if(C2$EC_SC[1] == 'EC'){ # assuming if one point is EC, the second is also EC
        ec.cal = TRUE
      } else {ec.cal = FALSE}  
        
      preCal<-CT_two_cal(data = C1, date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity,
                       high.Ref = C2$cond_uS, low.Ref = C2$cond_uS, 
                       startHigh = startHigh[1], endHigh = endHigh[1], 
                       startLow = startLow[1], endLow = endLow[1], EC_probe = probe.cal) %>% 
        rename(EC_Cal.1 = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
               TempInSitu_logger = TempInSitu, # rename logger's temperature readings column
               TempInSitu = TempInSitu_Cal) # rename calibrated temperature readings, calibrated to secondary probe, if available
        
    
    ## POSTCAL
    
      # specify whether calibration reference is an electrical conductivity or specific conductance value
      if(C2$EC_SC[3] == 'EC'){ # assuming if one point is EC, the second is also EC
        ec.cal = TRUE
      } else {ec.cal = FALSE}  
      
      postCal<-CT_two_cal(data = C1, date = C1$date, temp = C1$TempInSitu, EC = C1$E_Conductivity,
                          high.Ref = C2$cond_uS, low.Ref = C2$cond_uS, 
                          startHigh = startHigh[2], endHigh = endHigh[2], 
                          startLow = startLow[2], endLow = endLow[2], EC_probe = probe.cal) %>% 
        rename(EC_Cal.2 = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
               TempInSitu_logger = TempInSitu, # rename logger's temperature readings column
               TempInSitu = TempInSitu_Cal) %>%  # rename calibrated temperature readings, calibrated to secondary probe, if available
        
      
      calibration <- preCal %>% 
        full_join(postCal)
      
      # FALSE if double calibration times
      singleCal = FALSE
  } 
    
  
  # Calculate Practical Salinity using gsw package with PSS-78 equation
  if(singleCal == TRUE){
  calibration <- calibration %>%
    mutate(Salinity = gsw_SP_from_C(C = EC_Cal.1*0.001, t = TempInSitu, p = Pres_dbar))
  } #else {
  #   model <- lm(EC.Cal.1 ~ EC.Cal.2)
  # }
  
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
    filter(Serial == str_subset(launch.log$Serial, pattern = sn)) %>% 
    distinct()
  
  cg.start<-C2 %>% 
    filter(type == 'cg') %>% 
    select(time_start) %>% 
    mutate(time_start = ymd_hm(time_start))
  cg.end<-C2 %>% 
    filter(type == 'cg') %>% 
    select(time_end)%>% 
    mutate(time_end = ymd_hm(time_end))
  launch.start<-C2 %>% 
    filter(type == 'log') %>% 
    select(time_start)%>% 
    mutate(time_start = ymd_hm(time_start))
  launch.end<-C2 %>% 
    filter(type == 'log') %>% 
    select(time_end)%>% 
    mutate(time_end = ymd_hm(time_end))
    
  C1<-C1 %>% 
    filter(between(date, cg.start[1], cg.end[1]) |
           between(date, launch.start[1], launch.end[1]))
  
  # pull out original CT serial name
  sn <- CalLog %>% 
    filter(Serial == str_subset(CalLog$Serial, pattern = sn)) %>% # filter for i'th sn
    select(Serial)
  sn <- as.character(sn[1,]) # character string of full serial name
  
  C1 <- C1 %>% 
    mutate(Serial = sn)

  
  # Create Plot and save to list p
  p[[i]] <- C1 %>% 
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

