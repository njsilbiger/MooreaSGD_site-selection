##########################################################################
##########################################################################
#### pH Calibration script for HOBO pH logger
#### Brings in raw .csv files by Serial number and exports a tidy file

# Author: Danielle Barnas
# created: 9-23-2020
# modified: 8-10-2021 by Nyssa Silbiger

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


###################################
### File Paths
###################################

### Input
# Path to folder storing logger .csv files
path.log<-here("Data","June2022","Varari_Sled", "20220607","raw_files") # Logger in situ file path (CT and Water Level files)
file.date <- "20220607" # logger date used in file name(s)

### Output
# Path to store logger files
path.output<-here("Data","June2022","Varari_Sled", "20220607","QC_files") # Output file path


###################################
### Logger Serial Numbers
###################################

pH_Serial <- "196"

###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd('2022-06-07')
end.date <- ymd('2022-06-11')

# do you want to plot a graph?
plotgraph<-'yes'

###################################
### Import calibration and launch records
###################################

# Read in files that are updated with calibration and launch information
#calibration.log<-read_csv(here("Data","Tris_Calibration.csv")) # Calibration time logs
launch.log<-read_csv(here("Data","Launch_Log.csv")) %>%  # Launch time logs
  filter(Log_Type == "PH")



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

## in situ
## unite date to time columns and parse to POSIXct datetime

# THIS IS ALL DANIELLE'S CODE THAT I (NYSSA) CHANGED...
# launch.log <- launch.log %>% 
#   unite(col = 'start', date,start, sep = " ", remove = F) %>% 
#   unite(col = 'end', date,end, sep = " ", remove = F) %>% 
#   unite(col = 'cg.start', date,cg.start, sep = " ", remove = F) %>% 
#   unite(col = 'cg.end', date,cg.end, sep = " ", remove = F) %>% 
#   mutate(date = mdy(date)) %>% 
#   mutate(start = mdy_hms(start)) %>% 
#   mutate(end = mdy_hms(end)) %>% 
#   mutate(cg.start = mdy_hms(cg.start)) %>% 
#   mutate(cg.end = mdy_hms(cg.end))%>% 
#   filter(date == start.date | date == end.date) %>%  # filter only current launch dates
#   select(-date) # remove date column

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

# # Filter out in situ readings date and time 
# C2<-launch.log %>% 
#   filter(Serial == str_subset(launch.log$Serial, pattern = pH_Serial))
# 
# pHLog<-pH.data %>% 
#   filter(between(date, C2$cg.start[1], C2$cg.end[1]) |
#            between(date, C2$start[1], C2$end[nrow(C2)]))


if(plotgraph=='yes'){
# Plot pH data
p<-list()
p[[1]]<-pHLog %>% 
  ggplot(aes(x = date, y = pH, color = TempInSitu)) + 
  geom_line() + 
  theme_bw() +
  labs(x = "Date", color = "Temperature (C)") +
  ggtitle(paste("pH",pH_Serial))


# Save all plots in a single dated pdf
pdf(paste0(path.output,"/",file.date,"_pH_plot.pdf"), onefile = TRUE)
for (i in seq(length(p))) {
  tplot <- p[[i]]
  print(tplot)
}
dev.off()
}

# write out the clean data
write_csv(pHLog, paste0(path.output,"/QC_pH_",file.date,"_pH_",pH_Serial,".csv"))



