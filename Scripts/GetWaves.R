# Get wave watch 3 data
# By Nyssa Silbiger
# updated on 9/15/2021

library(rerddap)
library(lubridate)
library(tidyverse)
url<-"pae-paha.pacioos.hawaii.edu"


out <- info('ww3_global') # get the wave watch 3 info
res <- griddap(out,
               time = c('2021-5-1','2021-9-9'),
               latitude = c(-17.5, -17.5),
               longitude = c(210, 210),
               url = url) # get the gridded data

WaveData<-res$data %>%
  mutate(time = ymd_hms(time, tz = "UTC"),
         time = with_tz(time, "Pacific/Tahiti")) # convert time from UTC to tahit 



ggplot(WaveData, aes(x = time, y = shgt))+
  geom_line()
