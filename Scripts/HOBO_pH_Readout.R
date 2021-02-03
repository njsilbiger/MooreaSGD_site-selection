# Preprocessing script for hobo logger data
# Brings in sinlge raw .csv files and exports tidy files into both a general tidy folder for temporary storage and an output folder

# created: 9-23-2020 by Danielle Barnas
# modified: 9-29-2020

rm(list=ls())


library(tidyverse)
library(lubridate)
library(here)

here()

########################
# File Names
########################

file.date<-'011821' # Dated logger folder
Serial<-'197' # pH Probe Serial Number

# Date of in situ logs
Launch<-'2021-01-18 12:10:00' # Y-M-D H:M:S
Retrieval<-'2021-01-20 11:00:00' # Y-M-D H:M:S


#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

### Read in Logger Files
path.Log<-'Data/pH'
file.names.Log<-basename(list.files(path.Log, pattern = "csv$", recursive = F)) #list all csv file names in the folder and subfolders
file.names.Log<-file.names.Log[grep(pattern=Serial,x=file.names.Log)]
pHLog <- file.names.Log %>%
  map_dfr(~ read_csv(file.path(path.Log, .),skip=2,col_names=TRUE,col_types=list("Button Down"=col_skip(),"Button Up"=col_skip(),"Host Connect"=col_skip(),"Stopped"=col_skip(),"EOF"=col_skip())))
pHLog<-pHLog%>% # Filter specified probe by Serial number
  mutate(Serial=Serial)%>%
  rename(date=contains("Date"),TempInSitu=contains("Temp"))%>%
  drop_na()
#pHLog$date<-pHLog$date%>%parse_datetime(format = "%m/%d/%y %H:%M:%S %p", na = character(), locale = default_locale(), trim_ws = TRUE) # Convert 'date' to date and time vector type

# Parse date filters into date and type vector types
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

# Filter out dates
pHLog<-pHLog%>%filter(between(date,Launch,Retrieval)) 

# Plotting
pHLog%>%
  ggplot(aes(x=date,y=pH,color=TempInSitu))+
  geom_line()+
  ggsave(paste0('Output/timeseries_profiles/pH_',Serial,'_',file.date,'.png'), width = 15, height = 12)

# write file
write_csv(pHLog,paste0('Data/pH/timeseries/pH_',Serial,'_',file.date,'.csv'))


