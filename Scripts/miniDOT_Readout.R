# MiniDot Readout
# Updated by Nyssa Silbiger 
# 10/4/2021
# Uncalibrated for now


library(tidyverse)
library(lubridate)
library(here)


### Input
# Path to folder storing logger .csv files
path.log<-here("Data","August2021","Varari_Sled","20210811","raw_files", "miniDOT") # Logger in situ file path 
file.date <- "20210811" # logger date used in file name(s)

### Output
# Path to store logger files
path.output<-here("Data","August2021","Varari_Sled","20210811","QC_files") # Output file path


###################################
### Logger Serial Numbers
###################################

DO_Serial <- "658"

###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd('2021-08-11')
end.date <- ymd('2021-08-25')

# do you want to plot a graph?
plotgraph<-'no'

##############################
## DO NOT CHANGE ANYTHING BELOW
################################


###################################
### Import calibration and launch records
###################################

# Read in files that are updated with calibration and launch information
launch.log<-read_csv(here("Data","Launch_Log.csv")) %>%  # Launch time logs
  filter(Log_Type == "DO")%>%
  mutate(time_start = mdy_hm(time_start), # convert to time
         time_end = mdy_hm(time_end),
         start  = date(time_start), # extract the date
         end = date(time_end)) %>%
  filter(Serial == paste0("DO_",DO_Serial), # pull out the right serial number
         start == ymd(start.date),
         end == ymd(end.date))

# Tidy Concatenation Data
files <- dir(path = path.log, pattern = ".txt", full.names = TRUE)

DOData<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(~read_csv(., skip = 3, col_names = c("TimeSec","BatteryVolts","Temperature","DO_mg_L","Q"),
                   col_types = list("d","d","d","d","d"),),.id = "filename") %>%
  mutate(date = as_datetime(TimeSec, tz = "UTC")-hours(10)) %>%
  select(date, DO_mg_L, Temperature) %>%
  filter(date >launch.log$time_start & date < launch.log$time_end)


write_csv(DOData, file = here(path.output, paste0("DO_",file.date,".csv")))


ggplot(DOData, aes(x = date, y = DO_mg_L))+
  geom_line()
