##########################################################################
##########################################################################
#### Conductivity Calibration script for HOBO Conductivity-Temperature logger data
#### Brings in raw .csv files by Serial number and exports a tidy file

#### Reference: https://hasenmuellerlab.weebly.com/uploads/3/1/8/7/31874303/2019_shaughnessy_et_al_ema.pdf
#### Reference: https://www.aqion.de/site/112
#### Reference: https://oceanobservatories.org/wp-content/uploads/2015/10/1341-00040_Data_Product_SPEC_PRACSAL_OOI.pdf

# Author: Danielle Barnas  
# created: 9-23-2020
# modified: 5-2-2022

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


###################################
### File Paths
###################################

### Input
# Path to folder storing logger .csv files
path.log<-here("Data","Feb2023","Varari_Sled","2023-02-26", "raw_files") # Logger in situ file path (CT and Water Level files)
#path.WL<-here("Data","May2021","Depth")
file.date <- "2023-02-26" # date used in naming output file(s)
hobo.csv <- FALSE # TRUE if csv has been processed and calibrated through HOBOware
csv.pattern <- "CT" # file identifier at path.log (ex. "csv$")
ct.serial <- "353" # if isolating one CT logger

### Output
# Path to store logger files
path.output<-here("Data","Feb2023","Varari_Sled","2023-02-26", "QC_files") # Output file path


###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd_hm('2023-02-09 12:30')
end.date <- ymd_hm('2023-02-24 18:40')


###################################
### Import calibration and launch records
###################################

# Read in files that are updated with calibration and launch information
calibration.log<-read_csv(here("Data","CT_Calibration_Log.csv")) # Calibration time logs
launch.log<-read_csv(here("Data","Launch_Log.csv")) # Launch time logs


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

# CT_cleanup function pulled from 'mooreasgd' package
# Reads in all raw csv's and returns multiple tidied csv's

# Conductivity Calibration files, if different file path from in situ logs
# condCal<-CT_cleanup(data.path = path.cal, path.pattern = c(file.date,"csv$"), tf.write = F)

# In Situ Conductivity files
condLog<-CT_cleanup(data.path = path.log, output.path=path.output, path.pattern = csv.pattern, tf.write = F)

# Run if temperature may be in farenheit
condLog <- condLog %>%
  mutate(TempInSitu = if_else(TempInSitu > 40, ((TempInSitu - 32) *5/9), TempInSitu)) # mutate if temp is higher than anticipated celcius in field

############################################################
### Parse date and time
############################################################

### Parse dates into date and type vector types

## Calibration
## unite date to time columns and parse to POSIXct datetime
calibration.log <- calibration.log %>% 
  unite(col = 'time_in', date,time_in, sep = " ", remove = F) %>% # unite while maintaining separate date column
  unite(col = 'time_out', date,time_out, sep = " ", remove = F) %>% 
  mutate(date = mdy(date),
        time_in = mdy_hms(time_in), # assumes time is entered as hours and minutes (H:M) only
        time_out = mdy_hms(time_out)) %>% 
  filter(date == date(start.date) & pre_post == "pre" | 
           date == date(end.date) & pre_post == "post") # filter only current pre- and post-calibration dates

# potential filters for sandwich loggers
# calibration.log <- calibration.log %>%
#   filter(notes == "Cabral CTs") %>%
#   filter(notes != "Cabral Sled") %>%
#   filter(pre_post == "post") #%>%
#filter(LoggerID != "351") # Varari 3/18/2022

# if selecting single CT from same calibration date
if(exists('ct.serial') == T){
  calibration.log <- calibration.log %>% 
    filter(LoggerID == ct.serial)
}

## in situ
## unite date to time columns and parse to POSIXct datetime
launch.log <- launch.log %>%
  mutate(Date_launched = mdy_hm(time_start), # parse to date-time format
         Date_retrieved = mdy_hm(time_end)) %>% 
  filter(Date_launched == start.date & Date_retrieved == end.date) # filter only current launch dates
  #unite(col = "Time_launched",Date_launched,Time_launched, sep = " ", remove = F) %>% # reunite date and time columns
  #unite(col = "Time_retrieved",Date_retrieved,Time_retrieved, sep = " ", remove = F) 
# launch.log <- launch.log %>% 
#   filter(CowTagID != "Sled") #%>% 
  #filter(LoggerID != "351") # Varari 3/18/2022


# if selecting single CT from same calibration date
if(exists('ct.serial') == T){
  launch.log <- launch.log %>% 
    separate(Serial, into = c('loggerTyle', 'Serial')) %>% 
    filter(Serial == ct.serial)
}

if(hobo.csv == FALSE){
############################################################
### Calibration
############################################################

# get length of vector with all serial numbers for date(s) specified
n1 <- calibration.log %>%
  distinct(LoggerID) %>%
  nrow() %>%
  as.numeric()

# create empty df to add date-filtered data
CalLog<-tibble(date = as.POSIXct(NA),
               LoggerID = as.character(),
               FullLoggerID = as.character(),
               TempInSitu = as.numeric(),
               ECond.mS.cm = as.numeric(),
               Salinity_psu = as.numeric())

# Filter out calibration date and time and return dataframe with all logger calibration logs
for(i in 1:n1) {
  
  sn.a<-calibration.log %>% # vector of all serial numbers
    distinct(LoggerID) #%>% # pull each distinct Serial number
  #separate(col = 'LoggerID', into = c(NA,'LoggerID'), sep = "_")
  sn.a<-as.character(sn.a[i,]) # i'th serial number
  
  # filter by the i'th serial number
  C1.cal<-condLog %>% # C1: full in situ log; will be reduced to calibration logs
    filter(LoggerID == stringr::str_subset(condLog$LoggerID, pattern = sn.a))
  C2.cal<-calibration.log %>% # C2: shows calibration log relevant to LoggerID and datetime of calibration
    filter(LoggerID == stringr::str_subset(calibration.log$LoggerID, pattern = sn.a)) %>% 
    arrange(time_in) # order data by increasing date and time
  
  # pull out calibration times
  startHigh <- C2.cal %>%
    filter(cond_HL == 'H') %>% 
    select(time_in)
  endHigh <- C2.cal %>%
    filter(cond_HL == 'H') %>% 
    select(time_out)
  startLow <- C2.cal %>%
    filter(cond_HL == 'L') %>% 
    select(time_in)
  endLow <- C2.cal %>%
    filter(cond_HL == 'L') %>% 
    select(time_out)
  
  
  if(nrow(C2.cal) == 1){ 
    
    # specify whether calibration reference is an electrical conductivity or specific conductance (temp-compensated) value
    if(C2.cal$EC_SC == 'EC'){
      ec.cal = TRUE
    } else {ec.cal = FALSE}
    
    # single time point calibration
    calibration<-CT_one_cal(data = C1.cal,
                            cal.ref = C2.cal$cond_uS[1],
                            cal.ref.temp = C2.cal$temp_C[1],
                            startCal = startHigh,
                            endCal = endHigh,
                            date = C1.cal$date,
                            temp = C1.cal$TempInSitu,
                            EC.logger = C1.cal$E_Conductivity,
                            EC.cal = ec.cal) %>%
      select(date, LoggerID, TempInSitu_Cal, EC_Cal) %>% # only keep calibrated values and what we need
      rename(ECond.mS.cm = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
             TempInSitu = TempInSitu_Cal)  # rename calibrated temperature readings, calibrated to secondary probe, if available
    

  } else if(nrow(distinct(C2.cal,date,cond_HL)) == 2){
    
    # both High and Low calibration solutions present: two-point calibration
    if(nrow(distinct(C2.cal,cond_HL)) == 2){ 
      
      # specify whether calibration reference is an electrical conductivity or specific conductance value
      if(C2.cal$EC_SC[1] == 'EC'){ # assuming if one point is EC, the second is also EC
        ec.cal = TRUE
      } else {ec.cal = FALSE}  
      
      high.ref<-C2.cal %>% 
        filter(cond_HL == 'H') %>% 
        select(cond_uS)
      low.ref<-C2.cal %>% 
        filter(cond_HL == 'L') %>% 
        select(cond_uS)
      high.ref.temp<-C2.cal %>% 
        filter(cond_HL == 'H') %>% 
        select(temp_C)
      low.ref.temp<-C2.cal %>% 
        filter(cond_HL == 'L') %>% 
        select(temp_C)
      
      calibration<-CT_two_cal(data = C1.cal, 
                              high.ref = high.ref[1], 
                              low.ref = low.ref[1], 
                              high.ref.temp = high.ref.temp[1], 
                              low.ref.temp = low.ref.temp[1],
                              startHigh = startHigh[1,], 
                              endHigh = endHigh[2,], 
                              startLow = startLow[1,], 
                              endLow = endLow[2,],
                              date = C1.cal$date, 
                              temp = C1.cal$TempInSitu, 
                              EC.logger = C1.cal$E_Conductivity, 
                              EC.cal = ec.cal) %>% 
        rename(ECond.mS.cm = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
               TempInSitu_logger = TempInSitu, # rename logger's temperature readings column
               TempInSitu = TempInSitu_Cal)  # rename calibrated temperature readings, calibrated to secondary probe, if available
      
    } else if(nrow(distinct(C2.cal,cond_HL)) == 1){
      
      # only High or Low calibration solutions present: double calibration times at single calibration reference each time
      
      ## PRECAL
      # specify whether calibration reference is an electrical conductivity or specific conductance value
      if(C2.cal$EC_SC[1] == 'EC'){
        ec.cal = TRUE
      } else {ec.cal = FALSE}
      
      preCal<-CT_one_cal(data = C1.cal,
                         cal.ref = C2.cal$cond_uS[1],
                         cal.ref.temp = C2.cal$temp_C[1],
                         date = C1.cal$date,
                         temp = C1.cal$TempInSitu,
                         EC.logger = C1.cal$E_Conductivity,
                         startCal = startHigh[1,],
                         endCal = endHigh[1,],
                         EC.cal = ec.cal) %>%
     rename(EC_Cal.1 = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
             TempInSitu_logger.1 = TempInSitu, # rename logger's temperature readings column
             TempInSitu.1 = TempInSitu_Cal)  # rename calibrated temperature readings, calibrated to secondary probe, if available
      
      
      ## POSTCAL
      # specify whether calibration reference is an electrical conductivity or specific conductance value
      if(C2.cal$EC_SC[2] == 'EC'){
        ec.cal = TRUE
      } else {
        ec.cal = FALSE}
      
      postCal<-CT_one_cal(data = C1.cal,
                          cal.ref = C2.cal$cond_uS[2],
                          cal.ref.temp = C2.cal$temp_C[2],
                          date = C1.cal$date,
                          temp = C1.cal$TempInSitu,
                          EC.logger = C1.cal$E_Conductivity,
                          startCal = startHigh[2,],
                          endCal = endHigh[2,],
                          EC.cal = ec.cal) %>%
      
      rename(EC_Cal.2 = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
               TempInSitu_logger.2 = TempInSitu, # rename logger's temperature readings column
               TempInSitu.2 = TempInSitu_Cal)  # rename calibrated temperature readings, calibrated to secondary probe, if available
      
      
      calibration <- preCal %>% 
        full_join(postCal) 

      
      ### Drift Compensation
      drift<-calibration %>% 
        filter(between(date,startHigh[1,],endHigh[1,])) %>% # selects a calibration time point
        mutate(EC.drift.1 = EC_Cal.1 - E_Conductivity, # EC and Temperature drift between the pre and post calibration time points
               Temp.drift.1 = TempInSitu.1 - TempInSitu_logger.1,
               EC.drift.2 = EC_Cal.2 - E_Conductivity,
               Temp.drift.2 = TempInSitu.2 - TempInSitu_logger.2,
               drift.cor.ec = (EC.drift.2 - EC.drift.1)/length(calibration$date),   # Drift correction factor
               drift.cor.temp = (Temp.drift.2 - Temp.drift.1)/length(calibration$date))
      drift.cor.ec <- drift$drift.cor.ec[1]
      drift.cor.temp <- drift$drift.cor.temp[1]
      
      calibration <- calibration %>% 
        filter(between(date,startHigh[1,],endHigh[2,])) %>% # selects all data between calibration time points
        arrange(date) %>%
        mutate(drift.cor.ec.new = drift.cor.ec, # establish a column filled with the drift correction values
               drift.cor.temp.new = drift.cor.temp,
               drift.correction.ec=cumsum(drift.cor.ec.new), # fill the drift correction column with sequentially larger drift corrections from correlation value to full drift
               drift.correction.temp=cumsum(drift.cor.temp.new)) %>% 
        select(-drift.cor.ec.new,-drift.cor.temp.new) %>% 
        mutate(TempInSitu = TempInSitu.1 + drift.correction.temp,
               ECond.mS.cm = EC_Cal.1 + drift.correction.ec)
        
    }
  } else if(nrow(distinct(C2.cal,date,cond_HL)) == 4){
    
    # both High and Low calibration solutions present: two-point calibration
    # double time points for High and Low calibration: pre- and post- deployment calibrations
    
    ## PRECAL
    
    # specify whether calibration reference is an electrical conductivity or specific conductance value
    if(C2.cal$EC_SC[1] == 'EC'){ # assuming if one point is EC, the second is also EC
      ec.cal = TRUE
    } else {ec.cal = FALSE}  
    
    preCal<-CT_two_cal(data = C1.cal, 
                       date = C1.cal$date, 
                       temp = C1.cal$TempInSitu, 
                       EC.logger = C1$E_Conductivity,
                       high.Ref = C2.cal$cond_uS, 
                       low.Ref = C2.cal$cond_uS, 
                       startHigh = startHigh[1], 
                       endHigh = endHigh[1], 
                       startLow = startLow[1], 
                       endLow = endLow[1], 
                       EC.cal = probe.cal) %>% 
      rename(EC_Cal.1 = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
             TempInSitu_logger = TempInSitu, # rename logger's temperature readings column
             TempInSitu.1 = TempInSitu_Cal) # rename calibrated temperature readings, calibrated to secondary probe, if available
    
    
    ## POSTCAL
    
    # specify whether calibration reference is an electrical conductivity or specific conductance value
    if(C2.cal$EC_SC[3] == 'EC'){ # assuming if one point is EC, the second is also EC
      ec.cal = TRUE
    } else {ec.cal = FALSE}  
    
    postCal<-CT_two_cal(data = C1.cal, 
                        date = C1.cal$date, 
                        temp = C1.cal$TempInSitu, 
                        EC.logger = C1.cal$E_Conductivity,
                        high.Ref = C2.cal$cond_uS, 
                        low.Ref = C2.cal$cond_uS, 
                        startHigh = startHigh[2], 
                        endHigh = endHigh[2], 
                        startLow = startLow[2], 
                        endLow = endLow[2], 
                        EC.cal = probe.cal) %>% 
      rename(EC_Cal.2 = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
             TempInSitu_logger = TempInSitu, # rename logger's temperature readings column
             TempInSitu.2 = TempInSitu_Cal) %>%  # rename calibrated temperature readings, calibrated to secondary probe, if available
      
      
      calibration <- preCal %>% 
      full_join(postCal)
    
    
    ### Drift Compensation
    drift<-calibration %>% 
      filter(between(date,startHigh[1,],endHigh[1,])) %>% # selects a calibration time point
      mutate(EC.drift.1 = EC_Cal.1 - E_Conductivity, # EC and Temperature drift between the pre and post calibration time points
             Temp.drift.1 = TempInSitu.1 - TempInSitu_logger.1,
             EC.drift.2 = EC_Cal.2 - E_Conductivity,
             Temp.drift.2 = TempInSitu.2 - TempInSitu_logger.2,
             drift.cor.ec = (EC.drift.2 - EC.drift.1)/length(calibration$date),   # Drift correction factor
             drift.cor.temp = (Temp.drift.2 - Temp.drift.1)/length(calibration$date))
    drift.cor.ec <- drift$drift.cor.ec[1]
    drift.cor.temp <- drift$drift.cor.temp[1]
    
    calibration <- calibration %>% 
      filter(between(date,startHigh[1,],endHigh[2,])) %>% # selects all data between calibration time points
      arrange(date) %>%
      mutate(drift.cor.ec.new = drift.cor.ec, # establish a column filled with the drift correction values
             drift.cor.temp.new = drift.cor.temp,
             drift.correction.ec=cumsum(drift.cor.ec.new), # fill the drift correction column with sequentially larger drift corrections from correlation value to full drift
             drift.correction.temp=cumsum(drift.cor.temp.new)) %>% 
      select(-drift.cor.ec.new,-drift.cor.temp.new) %>% 
      mutate(TempInSitu = TempInSitu.1 + drift.correction.temp,
             ECond.mS.cm = EC_Cal.1 + drift.correction.ec)
    
  } # end of calibration if-else statements
  
#####################################################
  
  
  # Calculate Practical Salinity using gsw package with PSS-78 equation
  calibration <- calibration %>%
    mutate(Salinity_psu = gsw_SP_from_C(C = ECond.mS.cm*0.001, t = TempInSitu, p = Pres_dbar))
  
  # Create column for FullLoggerID vs LoggerID
  calibration <- calibration %>% 
    rename(FullLoggerID = LoggerID) %>% 
    mutate(LoggerID = sn.a) %>% 
    select(date, LoggerID, FullLoggerID, TempInSitu, ECond.mS.cm, Salinity_psu)
  
  CalLog <- CalLog %>% 
    rbind(calibration) # add i'th logger's data to running dataframe
}

} else { # end of calibration if hobo.csv = FALSE
  CalLog <- condLog %>%
    separate(col = 'LoggerID', into = c(NA,'ID'), sep = "_", remove = FALSE) %>%  # needs to be standardized - relies on consistent file naming
    rename(LoggerID = ID,
           FullLoggerID = LoggerID,
           ECond.mS.cm = E_Conductivity)
}

############################################################
### In Situ Logger Data
############################################################

n2 <- launch.log %>%
  distinct(Serial) %>%
  rename(LoggerID = Serial) %>% 
  nrow() %>%
  as.numeric()

# create empty df to add date-filtered data
Log <-tibble(date = as.POSIXct(NA),
             LoggerID = as.character(),
             FullLoggerID = as.character(),
             TempInSitu = as.numeric(),
             ECond.mS.cm = as.numeric(),
             Salinity_psu = as.numeric())

# create list for storing plots
p <- list()

# Pull out in situ logger data
for(i in 1:n2) {

  sn.b <- launch.log %>% # vector of all serial numbers
    distinct(Serial) #%>% 
  #separate(col = 'LoggerID', into = c(NA,'LoggerID'), sep = "_")
  sn.b <- as.character(sn.b[i,]) # i'th serial number
  
  # filter by the i'th serial number
  C1.log<-CalLog %>% 
    filter(LoggerID == str_subset(CalLog$LoggerID, pattern = sn.b)) #%>% 
  #mutate(LoggerID = paste0("CT_",sn.b)) # make serial the same for easy join
  C2.log<-launch.log %>% 
    filter(Serial == str_subset(launch.log$Serial, pattern = sn.b)) %>% 
    distinct()
  
  
  launch.start<-C2.log$Date_launched[[1]]
  launch.end<-C2.log$Date_retrieved[[1]]
  
  
  # pull out original CT serial name
  sn.b <- CalLog %>% 
    filter(LoggerID == str_subset(CalLog$LoggerID, pattern = sn.b)) %>% # filter for i'th sn
    select(LoggerID)
  sn.b <- as.character(sn.b[1,]) # character string of full serial name
  
  C1.log <- C1.log %>% 
    mutate(LoggerID = sn.b)
  
  
  
  C1.log <- C1.log %>% 
     filter(between(date, launch.start, launch.end))
  
  # Create Plot and save to list p
  p[[i]] <- C1.log %>% 
    ggplot(aes(x = date, y = Salinity_psu, color = TempInSitu)) + 
    geom_point() + 
    theme_bw() +
    labs(x = "Date", color = "Temperature (C)", y = "Salinity (psu)") + 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
    ggtitle(sn.b)
  
  
  #write.csv(C1.log, paste0(path.output,"/QC_",sn.b,".csv")) # write csv file for each Logger
  
  Log<-Log %>% 
    rbind(C1.log) %>% 
    distinct()
  
}

write_csv(Log, paste0(path.output,"/Full_CT_",file.date,".csv")) # write csv file for full set of logger data

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

