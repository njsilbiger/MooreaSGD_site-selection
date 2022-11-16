# PAR readout 
# created on 9/24/2021
# Nyssa Silbiger

###########

# load libraries #############
library(tidyverse)
library(here)
library(lubridate)

### Input
# Path to folder storing logger .csv files
path.log<-here("Data","August2022","Varari_Sled","20220822","raw_files", "PAR") # Logger in situ file path 
file.date <- "20220822" # logger date used in file name(s)

### Output
# Path to store logger files
path.output<-here("Data","August2022","Varari_Sled","20220822","QC_files") # Output file path


###################################
### Logger Serial Numbers
###################################

PAR_Serial <- "327"

###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd('2022-08-22')
end.date <- ymd('2022-08-24')

# do you want to plot a graph?
plotgraph<-'yes'

##############################
## DO NOT CHANGE ANYTHING BELOW
################################


###################################
### Import calibration and launch records
###################################

# Read in files that are updated with calibration and launch information
launch.log<-read_csv(here("Data","Launch_Log.csv")) %>%  # Launch time logs
  filter(Log_Type == "PAR")%>%
  mutate(time_start = mdy_hm(time_start), # convert to time
         time_end = mdy_hm(time_end),
         start  = date(time_start), # extract the date
         end = date(time_end)) %>%
  filter(Serial == paste0("PAR_",PAR_Serial), # pulll out the right serial number
         start == ymd(start.date),
         end == ymd(end.date))

# Tidy Concatenation Data
files <- dir(path = path.log,pattern = ".txt", full.names = TRUE)

PARData<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(~read_csv(., skip = 3, col_names = c("TimeSec","BatteryVolts","Temperature","PAR","Ax","Ay","Az"),
                   col_types = list("d","d","d","d","d","d","d"),),.id = "filename") %>%
  mutate(date = as_datetime(TimeSec, tz = "UTC")+days(61)+hours(1)-minutes(23)+seconds(31)) %>% # the dates were off....
  mutate(date = date-hours(10))%>% # keeps being off by 10 hours and I don't know why...
  select(date, PAR, Temperature)%>%
  filter(date >launch.log$time_start & date < launch.log$time_end)
  

write_csv(PARData, file = here(path.output, paste0("PAR_",file.date,".csv")))


ggplot(PARData, aes(x = date, y = PAR))+
  geom_line()



