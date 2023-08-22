

light_cleanup<-function (data.path, light.serial, output.path, tf_write = FALSE,  recursive_tf = FALSE) {
  file.names.Cal <- basename(list.files(data.path, pattern = c(light.serial, 
                                                               "csv$", recursive = recursive_tf)))
  LightLog <- file.names.Cal %>% purrr::map_dfr(~readr:::read_csv(file.path(data.path, 
                                                                         .), skip = 0, col_names = T))
  LightLog <- LightLog %>% dplyr::select(contains("Date"), 
                                   contains(light.serial), contains("Temp"), contains("lux")) %>% 
    dplyr::mutate(Serial = paste0("LUX_", light.serial)) %>% 
    dplyr::rename(date = contains("Date"), TempInSitu = contains("Temp"), Lux = contains("lux")) %>% 
    tidyr::drop_na()
  if (tf_write == TRUE) {
    write_csv(LightLog, paste0(output.path, "/Light_", 
                              Light.serial, "_tidy.csv"))
  }
  return(LightLog)
}


############
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
path.log<-here("Data","Feb2023","Varari_Sled","2023-04-21","raw_files") # Logger in situ file path (CT and Water Level files)
file.date <- "2023-04-21" # logger date used in file name(s)

### Output
# Path to store logger files
path.output<-here("Data","Feb2023","Varari_Sled","2023-04-21","QC_files") # Output file path


###################################
### Logger Serial Numbers
###################################

Light_Serial <- "841"

###################################
### Logger Launch and Retrieval dates
###################################

# Log dates
start.date <- ymd('2023-03-21')
end.date <- ymd('2023-04-12')

# do you want to plot a graph?
plotgraph<-'no'

###################################
### Import calibration and launch records
###################################

# Read in files that are updated with calibration and launch information
#calibration.log<-read_csv(here("Data","Tris_Calibration.csv")) # Calibration time logs
launch.log<-read_csv(here("Data","Launch_Log.csv")) %>%  # Launch time logs
  filter(Log_Type == "LUX")

############################################################
### Read in Logger Files
############################################################

# cleanup function pulled from 'mooreasgd' package
# Reads in raw csv and returns tidied csv for the probe with the specified serial number

# In Situ pH file
Light.data <- light_cleanup(data.path = path.log, light.serial = Light_Serial) %>% 
  rename(Light_Serial = Serial) %>%
  mutate(date=mdy_hms(date))

## parse the date ####
launch.log <- launch.log %>%
  mutate(time_start = mdy_hm(time_start), # convert to time
         time_end = mdy_hm(time_end),
         start  = date(time_start), # extract the date
         end = date(time_end)) %>%
  filter(Serial == paste0("LUX_",Light_Serial), # pull out the right serial number
         start == ymd(start.date),
         end == ymd(end.date))

############################################################
### In Situ Logger Data
############################################################
LightLog<-Light.data %>% # extract the data you need
  filter(between(date,launch.log$time_start, launch.log$time_end))

if(plotgraph=='yes'){
  # Plot pH data
  p<-list()
  p[[1]]<-LightLog %>% 
    ggplot(aes(x = date, y = Lux, color = TempInSitu)) + 
    geom_line() + 
    theme_bw() +
    labs(x = "Date", color = "Temperature (C)") +
    ggtitle(paste("Lux",Light_Serial))
  
  
  # Save all plots in a single dated pdf
  pdf(paste0(path.output,"/",file.date,"_Light_plots.pdf"), onefile = TRUE)
  for (i in seq(length(p))) {
    tplot <- p[[i]]
    print(tplot)
  }
  dev.off()
}

# write out the clean data
write_csv(LightLog, paste0(path.output,"/QC_Light_",Light_Serial,"_",file.date,".csv"))


