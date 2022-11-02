#plots for ADCP data
library(tidyverse)
library(here)
library(oce)
library(plotrix)
library(viridis)

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

  #  Scatterplots of Speed v Direction at each depth bin - outlier check

  p1<- d.csv.l %>% 
    ggplot(aes(x=Direction.tru, y=Speed)) +
    geom_point()+
    facet_wrap(vars(Distance))
  
  #check for instrument state
  # d.summary %>% 
  #   pivot_longer(cols=c(Pressure,Temperature,Pitch, Roll, Heading,
  #                       Direction, Speed, Northing, Easting, Alongshore,Cross_shore)) %>%
  #   ggplot(aes(x=DateTime)) +
  #   geom_point(y=) +
  # 
  # 
  #   p2a <- d.summary %>% 
  #   ggplot(aes(x=DateTime, y=Pressure))+
  #   geom_point()
  # p2b <- d.summary %>% 
  #   ggplot(aes(x=DateTime, y=Pressure))+
  #   geom_point()
  # p2c <- d.summary %>% 
  #   ggplot(aes(x=DateTime, y=Pressure))+
  #   geom_point()
  # p2d <- d.summary %>% 
  #   ggplot(aes(x=DateTime, y=Pressure))+
  #   geom_point()
  # p2e <- d.summary %>% 
  #   ggplot(aes(x=DateTime, y=Pressure))+
  #   geom_point()
  # p2f <- d.summary %>% 
  #   ggplot(aes(x=DateTime, y=Pressure))+
  #   geom_point()
  # 
    par(mfrow = c(7,1))
  oce.plot.ts(d.csv$DateTime,d.csv$Pressure,type="l",ylab = "Pressure")
  oce.plot.ts(d.csv$DateTime,d.csv$Temperature,type="l",ylab = "Temperature")
  oce.plot.ts(d.csv$DateTime,d.csv$Pitch,type="l",ylab = "Pitch")
  oce.plot.ts(d.csv$DateTime,d.csv$Roll,type="l",ylab = "Roll")
  oce.plot.ts(d.csv$DateTime,d.csv$Heading,type="l",ylab = "Heading")
  oce.plot.ts(d.summary$DateTime,ifelse(d.summary$Direction<180,d.summary$Direction+360,d.summary$Direction),
              type="l", ylab="Direction")
  oce.plot.ts(d.summary$DateTime,d.summary$Speed,type="l", ylab="Avg Speed")
  
  par(mfrow=c(1,1))
  d.summary.h %>% 
    ggplot(aes(x = DateTime, y = 0)) +
    geom_point()+
    geom_spoke(aes(angle = Direction*pi/180, radius = Speed))
  
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





