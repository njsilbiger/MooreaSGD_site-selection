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
library(oce)
library(plotrix)
library(viridis)

#declination for Mo'orea
declin <- 12.97
alongshore_heading <- c(306,346)  #northerly parallel to shore at (Varari, Cabral)

#do you want plots?
plot.me <- FALSE

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
  View(d.csv)
  #Truncate to usable data per metadata file
#  d.csv <- d.csv[d.csv$DateTime %within% interval(from, to),]
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
           crossshore = Speed*sin((Direction.shore)/360*2*pi),
           maj.dir = Speed*cos(scale(Direction.tru,center=h.mode,scale=FALSE)/360*2*pi),
           min.dir = Speed*sin(scale(Direction.tru,center=h.mode,scale=FALSE)/360*2*pi)
    )
  #View(d.csv.l)
  #find the modal direction
  h <- hist(d.csv.l$Direction.tru,90,plot=FALSE)
  h.mode <-h$mids[h$counts==max(h$counts)]
  
  #average easting and northing across depth; then back-transform to average speed & direction
  d.summary <-d.csv.l %>%
    group_by(DateTime) %>%
    summarize_at(vars(truN,truE,Pressure,Temperature,Heading,Pitch,Roll),mean) %>% 
    mutate(Direction.Avg = atan(truE/truN)*180/pi,Speed.Avg = sqrt(truE^2+truN^2)) %>% 
    rename(Northing = truN, Easting = truE)
  #View(d.summary)
  
  #write out files for each of the deployments
#  write.csv(d.summary,paste0(here("Data",trip,"ADCP",id),"/",id,"_summary.csv"))
  
  if (plot.me) {
  #check for instrument state
  par(mfrow = c(7,1))
  oce.plot.ts(d.csv$DateTime,d.csv$Heading,type="l",ylab = "Heading")
  oce.plot.ts(d.csv$DateTime,d.csv$Pitch,type="l",ylab = "Pitch")
  oce.plot.ts(d.csv$DateTime,d.csv$Roll,type="l",ylab = "Roll")
  oce.plot.ts(d.csv$DateTime,d.csv$Pressure,type="l",ylab = "Pressure")
  oce.plot.ts(d.csv$DateTime,d.csv$Temperature,type="l",ylab = "Temperature")
  oce.plot.ts(d.summary$DateTime,d.summary$Speed.Avg,type="l", ylab="Avg Speed")
  feather.plot(r=d.summary$Speed.Avg,theta=d.summary$Direction.Avg,
               fp.type="meteorological",use.arrows = FALSE)
  
  ##Plot speed and direction at 8 depth bins
  # par(mfrow=c(8,1))
  # for (i in seq(12,43,4)){
  #   oce.plot.ts(d.csv$DateTime,d.csv[,i],type="l",ylab = names(d.csv[i]))
  # }
  # par(mfrow=c(8,1))
  # for (i in seq(12,43,4)){
  #   feather.plot(r=d.csv[,i],theta=d.csv[,i+1],
  #                fp.type="meteorological",use.arrows = FALSE)
  # }
  
  par(mfrow=c(2,2))
  wr <- as.windrose(d.csv.l$truE,d.csv.l$truN)
  plot(wr)
  plot(d.csv.l$truN~d.csv.l$truE,type="p")
  hist(d.csv.l$truN)
  hist(d.csv.l$truE)
  
  par(mfrow=c(1,2))
  hist(d.csv.l$Speed,100)
  plot(d.csv.l$Speed ~ d.csv.l$Direction.tru)
  
  # par(mfrow=c(2,2))
  # wr <- as.windrose(d.csv.l$maj.dir,d.csv.l$min.dir)
  # plot(wr)
  # plot(d.csv.l$maj.dir~d.csv.l$min.dir,type="p")
  # hist(d.csv.l$maj.dir)
  # hist(d.csv.l$min.dir)
  
  #heatmaps of magnitude, northing, and easting over time (really should be made with facet)
  print(
    d.csv.l %>%
      ggplot(aes(x=DateTime, y=Distance, fill = Speed))+
      scale_fill_viridis()+
      geom_tile()
  )
  # print(
  #   d.csv.l %>%
  #     ggplot(aes(x=DateTime, y=Distance, fill = truN))+
  #     scale_fill_gradient2(
  #       low = "dark red",
  #       mid = "white",
  #       high = "dark blue",
  #       midpoint = 0,
  #       space = "Lab",
  #       na.value = "grey50",
  #       guide = "colourbar",
  #       aesthetics = "fill"
  #     )+
  #     geom_tile()
  # )
  # 
  # print(
  #   d.csv.l %>%
  #     ggplot(aes(x=DateTime, y=Distance, fill = truE))+
  #     scale_fill_gradient2(
  #       low = "dark red",
  #       mid = "white",
  #       high = "dark blue",
  #       midpoint = 0,
  #       space = "Lab",
  #       na.value = "grey50",
  #       guide = "colourbar",
  #       aesthetics = "fill"
  #     )+
  #     geom_tile()
  # )
  
  }
}




