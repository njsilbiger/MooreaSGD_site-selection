#plots for ADCP data
library(tidyverse)
library(here)
library(oce)
library(plotrix)
library(viridis)
library(lubridate)
library(scales)

#metadata for each deployment
meta <- read.csv(here("Data","ADCP_metadata.csv"))
#depth bins 
heights <- seq(0.15,0.95,0.05)

for (filenum in c(1:length(meta[,1]))) {
  
  site <- meta$Site[filenum]
  trip <- meta$Trip[filenum]
  int <- meta$Meas_Int[filenum]
  id <- meta$DeploymentID[filenum]
  d.csv <- read.csv(here("Data",trip,"ADCP",id,paste0(id,"_trim.csv")))
  d.csv.l <- read.csv(here("Data",trip,"ADCP",id,paste0(id,"_long.csv")))
  d.summary <- read.csv(here("Data",trip,"ADCP",id,paste0(id,"_summary.csv")))
  d.summary.h <- read.csv(here("Data",trip,"ADCP",id,paste0(id,"_summary_hourly.csv")))
  
  #if site is Varari, re-center the direction
  if(site=="Varari") {
    d.summary$Direction = ifelse(d.summary$Direction<120,d.summary$Direction+360,d.summary$Direction)
    d.csv.l$Direction.tru = ifelse(d.csv.l$Direction.tru<120,d.csv.l$Direction.tru+360,d.csv.l$Direction.tru)
  }
    
  #  Scatterplots of Speed v Direction at each depth bin - outlier check
  d.csv.l %>% 
    ggplot(aes(x=Direction.tru, y=Speed)) +
    geom_point()+
    facet_wrap(vars(Distance))
  
  #plot instrument state along with pressure, speed, direction
  d.summary %>%
    pivot_longer(cols=c(Pressure,Temperature,Pitch, Roll, Heading,
                        Direction, Speed),
                 names_to="Variable",values_to="Value") %>%
    ggplot(aes(x=as.POSIXct(DateTime),y=Value)) +
    geom_point() +
    facet_grid(rows=vars(factor(Variable, 
                                c("Pressure","Speed", "Direction", "Temperature", "Heading","Pitch","Roll"),
                                labels=c("Pressure","Speed m/s", "Direction (deg)", "Temp (C)", "Heading","Pitch","Roll"))),
              scales="free_y") +
    labs(x="",y="") +
    scale_x_datetime(breaks=date_breaks("1 day")) +
    theme_bw()
 
  #plot pressure and temp only
    d.summary %>% pivot_longer(cols=c(Pressure,Temperature),
                 names_to="Variable",values_to="Value") %>%
    ggplot(aes(x=as.POSIXct(DateTime),y=Value)) +
    geom_point() +
    facet_grid(rows=vars(factor(Variable, 
                                c("Pressure","Temperature"),
                                labels=c("Pressure (atm)","Temperature (C)"))),
               scales="free_y") +
    labs(x="",y="") +
    scale_x_datetime(breaks=date_breaks("1 day")) +
    theme_bw()
  
  #plot speed, direction with binned depths
  d.csv.l %>% 
    ggplot(aes(x=as.POSIXct(DateTime),y=Distance)) +
    geom_tile(aes(fill=Speed))+
    labs(x="",y="Distance from Bottom") +
    scale_x_datetime(breaks=date_breaks("1 day")) +
    guides(fill=guide_legend(title="Current (m/s)")) +
    theme_bw()
  
  d.csv.l %>% 
    ggplot(aes(x=as.POSIXct(DateTime),y=Distance)) +
    geom_tile(aes(fill=Direction.tru))+
    labs(x="Date",y="Distance from Bottom") +
    scale_x_datetime(breaks=date_breaks("1 day")) +
    guides(fill=guide_legend(title="Direction (°)")) +
    theme_bw()

  # #feather plot for current speed and direction
  # d.summary.h$DateTime <- as.POSIXct(d.summary.h$DateTime)
  # d.summary.h %>% 
  #   # mutate(yend = Speed * cos(Direction * 2 * pi / 360)*.1,
  #   #        xend = dhours((d.summary.h$Speed * sin(d.summary.h$Direction * 2 * pi / 360))*.1)+as_date(d.summary.h$DateTime)) %>%
  #   # ggplot(aes(x = as.POSIXct(DateTime),y=0)) +
  #   ggplot() +
  #   geom_point(aes(DateTime,y=0))+
  #   geom_segment(aes(x = DateTime,
  #                    y=0,
  #                    yend = Speed*-sin(Direction * 2 * pi / 360),
  #                    xend = dhours(Speed * -cos(90-Direction)/360 * 2 * pi)+DateTime),
  #                arrow=arrow(length=unit(0.5,"cm"))) +
  #   theme_bw()

  # #feather plot version
  # feather.plot(r=d.summary.h$Speed,theta=d.summary.h$Direction *pi/180,xpos=as.POSIXct(d.summary.h$DateTime),yref=0,use.arrows=TRUE,
  #              col.refline="lightgray",fp.type="s",main="",xlab="",ylab="",
  #              xlabels=NULL)
  # feather.plot(r=.1,theta=d.summary.h$Direction *pi/180,xpos=as.POSIXct(d.summary.h$DateTime),yref=d.summary.h$Speed,
  #              use.arrows=TRUE,col.refline="white",fp.type="m",
  #              main="",xlab="",ylab="", xlabels=NULL)
  # points(y=d.summary.h$Speed,x=as.POSIXct(d.summary.h$DateTime),col="blue")
  # lines(y=d.summary.h$Speed,x=as.POSIXct(d.summary.h$DateTime),col="blue")
  
  par(mfrow=c(2,2))
  wr <- as.windrose(d.summary$Easting,d.summary$Northing)
  plot(wr,convention="m",type="count")
  hist(d.summary$Easting,main="Easting")
  hist(d.summary$Northing,main="Northing")
  plot(d.summary$Northing~d.summary$Easting,type="p",main="Northing by Easting")
  
  par(mfrow=c(1,2))
  hist(d.summary$Speed,100,main="Current Speed, m/s")
  plot(d.summary$Speed ~ d.summary$Direction, xlab="Direction", ylab = "Speed")
  
  #heatmaps of magnitude, northing, and easting over time 
  
  d.csv.l %>%
      ggplot(aes(x=DateTime, y=Distance, fill = Speed))+
      scale_fill_viridis()+
      geom_tile()
  d.csv.l %>%
    ggplot(aes(x=DateTime, y=Distance, fill = Direction.tru))+
    scale_fill_viridis()+
    geom_tile()

    d.csv.l %>%
      ggplot(aes(x=DateTime, y=Distance, fill = truN))+
      scale_fill_gradient2(
        low = "dark red",
        mid = "white",
        high = "dark blue",
        midpoint = 0,
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "fill"
      )+
      geom_tile()

    d.csv.l %>%
      ggplot(aes(x=DateTime, y=Distance, fill = truE))+
      scale_fill_gradient2(
        low = "dark red",
        mid = "white",
        high = "dark blue",
        midpoint = 0,
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "fill"
      )+
      geom_tile()





