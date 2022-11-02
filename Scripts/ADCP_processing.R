#Using data from Nortek software that is exported in csv format.  This data is given in Speed and Direction by distance bin.
# Need to correct for declination, rearrange, and average across depths
# information on ADCP files is given in ADCP_metadata.csv
# Note that the deployments from August 2021 and Mar 2022 have the same parameters
# but some sampled at 10 minute intervals and some sampled at 2 minute intervals

# Nortek Aquadopp Profiler ASP2348 was purchased 03/20/2007 by F. Thomas; inherited in 2016
# The seven deployments from August 2021 and May 2022 all measured the water
# column from 15cm above bottom to 1m above the bottom.  This probably underestimates
# flow speeds, which likely were faster closer to the surface.
# 
# For methods section:   
#   An upward-looking acoustic Doppler current profiler (ADCP) (Nortek Aquadopp Profiler HR 2MHz) 
# was weighted and placed in a sand channel in 1.5-2m of water at least 5m from the nearest 
# patch reef.  The ADCP sampled from 0.15 to 1.0m above the seafloor for 60 seconds at 2 Hz 
# every 2 or 10 minutes (depending on deployment length).  We calculated current measurements by 
# averaging all samples in a burst and averaging over the full depth to produce a Eulerian velocity 
# timeseries of depth averaged current speed (m/s) and direction.

library(tidyverse)
library(here)
library(lubridate)

#do you want to write out new copies of the data?
write.out <- FALSE

#declination for Mo'orea
declin <- 12.97
alongshore_heading <- c(306,346)  #northerly parallel to shore at (Varari, Cabral)

meta <- read.csv(here("Data","ADCP_metadata.csv"))
meta$Begin <- as.POSIXct(mdy_hms(meta$Begin,tz="Pacific/Tahiti",truncated=1))
meta$End <- as.POSIXct(mdy_hms(meta$End,tz="Pacific/Tahiti",truncated=1))
meta$Data_start <- as.POSIXct(mdy_hms(meta$Data_start,tz="Pacific/Tahiti",truncated=1))
meta$Data_stop <- as.POSIXct(mdy_hms(meta$Data_stop,tz="Pacific/Tahiti",truncated=1))
#View(meta)

for (filenum in c(1:length(meta[,1]))) {
  
  site <- meta$Site[filenum]
  trip <- meta$Trip[filenum]
  id <- meta$DeploymentID[filenum]
  from <- meta$Data_start[filenum]
  to <- meta$Data_stop[filenum]
  int <- meta$Meas_Int[filenum]
  longshore_offset <- 360 - ifelse(meta$Site[filenum]=="Varari",alongshore_heading[1],alongshore_heading[2])
  
  d.csv <- read.csv(paste0(here("Data",trip,"ADCP",id),"/",id,".csv"),sep=";")
  d.csv$DateTime <- as.POSIXct(mdy_hms(d.csv$DateTime,truncated=1,tz="Pacific/Tahiti"))
  #View(d.csv)
  #Truncate to usable data per metadata file
  d.csv <- d.csv %>% filter(DateTime %within% interval(from, to))

  #long-form data
  d.csv.l <-d.csv %>% 
    pivot_longer(
      cols = c(contains('Speed'),contains('Dir'))
    ) %>% 
    mutate(meas = if_else(str_detect(name,'Speed'),'Speed','Direction')) %>% 
    mutate(Distance = as.numeric(str_sub(name,start=-7, end=-3))) %>% 
    #select(!c("AnalogIn1","AnalogIn2","X","name")) %>% 
    select(!c("AnalogIn1":"name")) %>% 
    pivot_wider(names_from=c("meas"))
  
  #Decompose into northing and easting in each depth bin:datetime
  d.csv.l <-d.csv.l %>% 
    rename(Direction.mag = Direction) %>% 
    mutate(Direction.tru = Direction.mag + declin,
           Direction.shore = Direction.tru + longshore_offset,
           magN = Speed*cos(Direction.mag/360*2*pi),
           magE = Speed*sin(Direction.mag/360*2*pi),
           truN = Speed*cos((Direction.tru)/360*2*pi),
           truE = Speed*sin((Direction.tru)/360*2*pi),
           longshore = Speed*cos((Direction.shore)/360*2*pi),
           crossshore = Speed*sin((Direction.shore)/360*2*pi)
    )
  #View(d.csv.l)
  #find the modal direction
  h <- hist(d.csv.l$Direction.tru,90,plot=FALSE)
  h.mode <-h$mids[h$counts==max(h$counts)]
  d.csv.l$maj.dir = d.csv.l$Speed*cos(scale(d.csv.l$Direction.tru,center=h.mode,scale=FALSE)/360*2*pi)
  d.csv.l$min.dir = d.csv.l$Speed*sin(scale(d.csv.l$Direction.tru,center=h.mode,scale=FALSE)/360*2*pi)
  
  #average easting and northing across depth; then back-transform to average speed & direction
  d.summary <-d.csv.l %>%
    group_by(DateTime) %>%
    summarize_at(vars(truN,truE,longshore,crossshore,Pressure,Temperature,Heading,Pitch,Roll),mean) %>% 
    mutate(Direction = atan(truE/truN)*180/pi,Speed = sqrt(truE^2+truN^2)) %>%
    mutate(Direction = ifelse (Direction<0,306+Direction,Direction)) %>% 
    rename(Northing = truN, Easting = truE, Alongshore = longshore, Cross_shore = crossshore)
  #View(d.summary)

  #make hourly summary for plotting; center around the hour (so subtract 30 min)
  d.summary.h <- d.summary %>% 
    group_by(Date=date(DateTime-minutes(30)),Hour = hour(DateTime-minutes(30))) %>% 
    summarize_at(vars(Northing, Easting, Alongshore, Cross_shore,Pressure, Temperature, Direction,Speed),mean) %>% 
    mutate(DateTime = ymd_hm(paste(Date,Hour,":00"))) %>%
    ungroup %>% 
    select(DateTime, Pressure, Temperature, Direction, Speed, Northing, Easting, Alongshore, Cross_shore)
  
  if (write.out==TRUE){
    #write out files for each of the deployments
    write.csv(d.csv,paste0(here("Data",trip,"ADCP",id),"/",id,"_trim.csv"))
    write.csv(d.csv.l,paste0(here("Data",trip,"ADCP",id),"/",id,"_long.csv"))
    write.csv(d.summary,paste0(here("Data",trip,"ADCP",id),"/",id,"_summary.csv"))
    write.csv(d.summary.h,paste0(here("Data",trip,"ADCP",id),"/",id,"_summary_hourly.csv"))
  }
}
