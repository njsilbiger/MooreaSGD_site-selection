#plots for ADCP data
library(tidyverse)
library(here)
library(oce)
library(plotrix)
library(viridis)
library(lubridate)
library(scales)
library(patchwork)

########PLOTS FOR PAPER
d.adcp <- read.csv(here("Data","ADCP","ADCP_all.csv"))
d.adcp <-d.adcp %>% 
  mutate(Direction360 = if_else(Direction<0, Direction + 360,Direction))

#View(d.adcp)
d.adcp.plots <- d.adcp %>% 
  filter(ID %in% c("210808","220329","210804","220320"))

d.adcp %>% 
  ggplot(aes(x=Speed,group=Trip, fill = Trip)) +
  geom_density(alpha=0.5)+
  labs(x = "Speed (m/s)",
       y = "Density")+
  facet_wrap(~Site, ncol=1, scales = "free_y")+
  theme_bw()

source(here("Scripts","ADCP_windrose.R"))

#Varari
#for (id in c("210804","220320")) {
  v1<-  plot.windrose(spd = d.adcp$Speed[d.adcp$ID=="210804"],
              dir = d.adcp$Direction360[d.adcp$ID=="210804"],
              spdres = 0.05,
              dirres = 30,
              spdmin = 0,
              spdmax = 0.45,
              palette = "YlGnBu",
              countmax = NA,
              debug = 0)+
    labs(x = "",
         y = "")+
    theme_linedraw()+
    scale_y_continuous(limits = c(0,1500))+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_line(color = "grey"))
#}

  v2<-  plot.windrose(spd = d.adcp$Speed[d.adcp$ID=="220320"],
                      dir = d.adcp$Direction360[d.adcp$ID=="220320"],
                      spdres = 0.05,
                      dirres = 30,
                      spdmin = 0,
                      spdmax = 0.45,
                      palette = "YlGnBu",
                      countmax = NA,
                      debug = 0)+
    labs(x = "",
         y = "")+
    theme_linedraw()+
    scale_y_continuous(limits = c(0,1500))+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_line(color = "grey"))
  #}
v1+v2+plot_layout(guides = "collect")
  
#Cabral
#for (id in c("210808","220329")) {
c1<-  plot.windrose(spd = d.adcp$Speed[d.adcp$ID=="210808"],
                dir = d.adcp$Direction360[d.adcp$ID=="210808"],
                spdres = 0.05,
                dirres = 30,
                spdmin = 0,
                spdmax = 0.45,
                palette = "YlGnBu",
                countmax = NA,
                debug = 0)+
    labs(x = "",
         y = "")+
  scale_y_continuous(limits = c(0,500))+
    theme_linedraw()+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_line(color = "grey"))

c2<-  plot.windrose(spd = d.adcp$Speed[d.adcp$ID=="220329"],
                    dir = d.adcp$Direction360[d.adcp$ID=="220329"],
                    spdres = 0.05,
                    dirres = 30,
                    spdmin = 0,
                    spdmax = 0.45,
                    palette = "YlGnBu",
                    countmax = NA,
                    debug = 0)+
  labs(x = "",
       y = "")+
  scale_y_continuous(limits = c(0,500))+
  theme_linedraw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_line(color = "grey")
        )
#}

c1+c2+plot_layout(guides = "collect")

(c1+c2)/(v1+v2)+plot_layout(guides = "collect")&theme(panel.background = element_blank())

ggsave(here("Output","Windroseplots.pdf"), width = 5, height = 5)
# 
# d.adcp %>% 
#   mutate(SiteTripID = paste0(Site,"_",ID)) %>% 
#   #filter(Site=="Cabral") %>% 
#   ggplot(aes(Speed)) +
#   geom_histogram() +
#   facet_grid(rows="SiteTripID")
# d.adcp %>% 
#   mutate(SiteTripID = paste0(Site,"_",ID)) %>% 
#   ggplot(aes(x=DateTime,y=Speed)) +
#   geom_point() +
#   facet_wrap("SiteTripID",scales="free")
######OTHER PLOTS##############



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
  plot(d.summary$Northing~d.summary$Easting,type="p",main=paste(site,trip))
  
  par(mfrow=c(1,2))
  hist(d.summary$Speed,100,main="Current Speed, m/s")
  plot(d.summary$Speed ~ d.summary$Direction, 
       xlab="Direction", ylab = "Speed", main=paste(site,trip))
  
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

}


  