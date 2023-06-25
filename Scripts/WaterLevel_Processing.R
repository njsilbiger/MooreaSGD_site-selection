##########################################################################
##########################################################################
#### Depth Processing script for HOBO Water Level data pre-processed in HOBOware software
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

###################################
### File Paths
###################################

### Input
# Path to folder storing logger .csv files
path.WL<-here("Data","May2023","Varari_Sled","raw_files")
file.date <- "20230617" # logger date for plot

### Output
# Path to store logger files
file.output<-here("Data","May2023","Varari_Sled","weekday_20230617") # Output file path; Spatial vs Timeseries survey
#fig.output<-here("Data","August2021","Varari_Sled","20210825")

###################################
### Logger Serial Numbers
###################################

WL_Serial <- "870"

###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd('2023-06-11')
end.date <- ymd('2023-06-17')

###################################
### Import calibration and launch records
###################################

# Read in files that are updated with launch date and time
launch.log<-read_csv(here("Data","Launch_Log.csv")) %>%  # Launch time logs
  filter(Log_Type == "WL")

#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

############################################################
### Read in Calibration and Logger Files
### Temperature compensation (source: https://www.aqion.de/site/112)
############################################################

# cleanup function pulled from 'mooreasgd' package
# Reads in raw csv and returns tidied csv for the probe with the specified serial number


# In Situ Pressure/Depth file
presLog <- WL_cleanup(data.path = path.WL, path.pattern = "WL") %>% 
  rename(LoggerID = Serial)

############################################################
### Parse date and time
############################################################

### Parse dates into date and type vector types

## in situ
## unite date to time columns and parse to POSIXct datetime
launch.log <- launch.log %>% 
  mutate(time_start = mdy_hm(time_start)) %>% 
  mutate(time_end = mdy_hm(time_end)) %>% 
  separate(col = time_start, into = c("date", NA), sep = " ", remove = F) %>% 
  filter(date == start.date | date == end.date) %>%  # filter only current launch dates
  select(-date) # remove date column


############################################################
### In Situ Logger Data
############################################################


# Filter out in situ readings date and time 
launch.log<-launch.log %>% 
  filter(Serial == str_subset(launch.log$Serial, pattern = WL_Serial))  

presLog<-presLog %>% 
  filter(between(date, launch.log$time_start[[1]], launch.log$time_end[[1]]))

# Plot Depth data
# p<-list()
 presLog %>%
  ggplot(aes(x = date, y = Depth, color = TempInSitu)) +
  geom_line() +
  scale_y_reverse() +
  theme_bw() +
  labs(x = "Date", color = "Temperature (C)", y = "Depth (m)") +
  ggtitle(paste("WL",WL_Serial,"Depth", file.date)) 
 
# # Save all plots in a single dated pdf
# pdf(paste0(fig.output,"/Depth_plot",file.date,"_WL_",WL_Serial,".pdf"), onefile = TRUE)
# for (i in seq(length(p))) {
#   tplot <- p[[i]]
#   print(tplot)
# }
# dev.off()


write_csv(presLog, paste0(file.output,"/QC_WL_",file.date,"_WL_",WL_Serial,".csv"))


