##########################################################################
##########################################################################
#### pH Calibration script for HOBO pH logger
#### Brings in raw .csv files by Serial number and exports a tidy file

# Author: Danielle Barnas
# created: 9-23-2020
# modified: 8-10-2021 by Nyssa Silbiger

##########################################################################
##########################################################################

pH_cleanup <- function (data.path, pH.serial, output.path, tf_write = FALSE, 
          recursive_tf = FALSE) 
{
  file.names.Cal <- basename(list.files(data.path, pattern = c(pH.serial, 
                                                               "csv$", recursive = recursive_tf)))
  pHLog <- file.names.Cal %>% purrr::map_dfr(~readr:::read_csv(file.path(data.path, 
                                                                         .), skip = 0, col_names = T))
  pHLog <- pHLog %>% 
    dplyr::select(contains("Date"), 
                  contains(pH.serial), 
                  contains("Temp"), 
                  contains("mV"),
                  contains("pH")) %>% 
    dplyr::mutate(Serial = paste0("pH_", 
                                  pH.serial)) %>% 
    dplyr::rename(date = contains("Date"), 
                  TempInSitu = contains("Temp"),
                  mV = contains("mv"),
                  pH = contains("pH")) %>% 
    tidyr::drop_na()
  
  if (tf_write == TRUE) {
    write.csv(pHLog, paste0(output.path, "/pH_", pH.serial, "_tidy.csv"))
  }
  return(pHLog)
}



###################################
### Load Libraries
###################################

#devtools::install_github("dbarnas/mooreasgd") # if package has updated since last run
library(tidyverse)
library(lubridate)
library(gsw)
library(here)
library(gridExtra)
#library(mooreasgd)


###################################
### File Paths
###################################

### Input
# Path to folder storing logger .csv files
path.log<-here("Data","Feb2023","Varari_Sled","2023-03-20","raw_files") # Logger in situ file path (CT and Water Level files)
file.date <- "2023-03-20" # logger date used in file name(s)

### Output
# Path to store logger files
path.output<-here("Data","Feb2023","Varari_Sled","2023-03-20","QC_files") # Output file path


###################################
### Logger Serial Numbers
###################################

pH_Serial <- "195"

###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd('2023-02-27')
end.date <- ymd('2023-03-17')

# do you want to plot a graph?
plotgraph<-'no'

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
  rename(pH_Serial = Serial)  %>%
  mutate(date=mdy_hms(date))


############################################################
### Parse date and time
############################################################

### Parse dates into date and type vector types

## in situ
## unite date to time columns and parse to POSIXct datetime


launch.log <- launch.log %>%
  mutate(time_start = mdy_hm(time_start), # convert to time
         time_end = mdy_hm(time_end),
         start  = date(time_start), # extract the date
         end = date(time_end)) %>%
    filter(Serial == paste0("PH_",pH_Serial), # pull out the right serial number
         start == ymd(start.date),
         end == ymd(end.date))

############################################################
### In Situ Logger Data
############################################################
pHLog<-pH.data %>% # extract the data you need
  filter(between(date,launch.log$time_start, launch.log$time_end))


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
write_csv(pHLog, paste0(path.output,"/QC_pH_",pH_Serial,"_",file.date,".csv"))



