#Plot YSI data
# MJD 14 May 2021

setwd("C:/Users/Megan/Documents/GitHub/MooreaSGD_site-selection")

library(tidyverse)
library(lubridate)
library(viridis)

d <- read_csv("Data/May2021/YSI/20210513_YSI_trimmed.csv")#,col_types = "??cc?????????????")

d$time <- hms::as_hms(d$TIME)
d$date <- mdy(d$DATE)
d$Salinity <- d$Salinity -6.64  #correcting salinity calibration
Sal_filter <- 28

View(d)

filter(d, (LAT < 149 & LONG < 17 & Who=="Megan")) %>%
  ggplot() +
  geom_histogram(aes(x=Salinity))

filter(d, (LAT < 149 && LONG < 17 && Who=="Megan")) %>%
  ggplot() +
  geom_point(aes(x=TempC, y=Salinity, color=time))

filter(d, (Who =="Megan" & Salinity >Sal_filter & 
             between(LONG,-149.89975,-149.89925))) %>%
  ggplot() +
  geom_point(mapping=aes(x=LONG,y=LAT,color=Salinity))


filter(d, (Who =="Megan" & Salinity >Sal_filter)) %>%
  ggplot() +
  #scale_colour_viridis(option = "D",direction=-1,begin =0.15,end=0.85) +
  geom_point(mapping=aes(x=LONG,y=LAT,color=Salinity))+
  geom_point(mapping=aes(y=-17.540825,x=-149.899208),color="red")+
  geom_point(mapping=aes(y=-17.540357,x=-149.899207),color="purple")+
  geom_point(mapping=aes(y=-17.541395,x=-149.897587),color="green")

filter(d, (Who =="Megan" & Salinity >Sal_filter)) %>%
  ggplot() +
  #scale_colour_viridis(option = "D",direction=-1,begin =0.15,end=0.85) +
  geom_point(mapping=aes(x=LONG,y=LAT,color=TempC)) +
  geom_point(mapping=aes(y=-17.540825,x=-149.899208),color="red")+
  geom_point(mapping=aes(y=-17.540357,x=-149.899207),color="purple")+
  geom_point(mapping=aes(y=-17.541395,x=-149.897587),color="green") 


filter(d, (Who =="Megan" & Salinity >Sal_filter)) %>%
  ggplot() +
  #scale_colour_viridis(option = "D",direction=-1,begin =0.15,end=0.85) +
  geom_point(mapping=aes(x=LONG,y=LAT,color=as.numeric(TIME)))+
  geom_point(mapping=aes(y=-17.540825,x=-149.899208),color="red")+
  geom_point(mapping=aes(y=-17.540357,x=-149.899207),color="purple")+
  geom_point(mapping=aes(y=-17.541395,x=-149.897587),color="green")

filter(d, (Who =="Megan" & Salinity >Sal_filter)) %>%
  ggplot() +
  geom_point(mapping=aes(x=Salinity,y=TempC))

