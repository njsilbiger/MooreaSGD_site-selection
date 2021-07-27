## 1/15/2021 CT files


# created: 1-20-2020 by Danielle Barnas


library(tidyverse)
library(cowplot)

date<-'4Feb21' # today's date

## File names for ggsave need to be changed before running script
sled316<-read_csv('Data/Cond_temp/Calibrated_files/timeseries/0117-1921_CT316_sled_2pcal_withCGcorr.csv')
sen318<-read_csv('Data/Cond_temp/Calibrated_files/timeseries/0117-1921_CT318_insitu_2pcal_withCGcorr.csv')
sen319<-read_csv('Data/Cond_temp/Calibrated_files/timeseries/0117-1921_CT319_insitu_2pcal_withCGcorr.csv')
sen338<-read_csv('Data/Cond_temp/Calibrated_files/timeseries/0117-1921_CT338_insitu_2pcal_withCGcorr.csv')

sled324<-read_csv('Data/Cond_temp/Calibrated_files/timeseries/0118-2021_CT324_sled_2pcal_withCGcorr.csv')
sen337<-read_csv('Data/Cond_temp/Calibrated_files/timeseries/0118-2021_CT337_insitu_2pcal_withCGcorr.csv')
sen343<-read_csv('Data/Cond_temp/Calibrated_files/timeseries/0118-2021_CT343_insitu_2pcal_withCGcorr.csv')
sen331<-read_csv('Data/Cond_temp/Calibrated_files/timeseries/0118-2021_CT331_insitu_2pcal_withCGcorr.csv')

## Plotting
Launch.17<-parse_datetime('2021-01-17 12:30:00',format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval.19<-parse_datetime('2021-01-19 09:30:00',format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Launch.18<-parse_datetime('2021-01-18 14:00:00',format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval.20<-parse_datetime('2021-01-20 11:00:00',format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

## Plotting Salinities
a.plot<-sled316%>%
  filter(between(date,Launch.17,Retrieval.19))%>%
  ggplot(aes(x=date,y=SalinityInSitu_2pCal_wOffset,color=TempInSitu))+
  geom_line()
b.plot<-sen318%>%
  filter(between(date,Launch.17,Retrieval.19))%>%
  ggplot(aes(x=date,y=SalinityInSitu_2pCal_wOffset,color=TempInSitu))+
  geom_line()
c.plot<-sen319%>%
  filter(between(date,Launch.17,Retrieval.19))%>%
  filter(SalinityInSitu_2pCal_wOffset>27)%>%
  ggplot(aes(x=date,y=SalinityInSitu_2pCal_wOffset,color=TempInSitu))+
  geom_line()
d.plot<-sen338%>%
  filter(between(date,Launch.17,Retrieval.19))%>%
  ggplot(aes(x=date,y=SalinityInSitu_2pCal,color=TempInSitu))+
  geom_line()

ggdraw() +
  draw_plot(a.plot, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(b.plot, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(c.plot, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(d.plot, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("sled316", "sn318", "sn319", "sn338"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5)) +
  ggsave(paste0('Output/Daily_CT_Profiles/0117-1921_CT_profiling_',date,'.pdf'), width = 15, height = 12)

e.plot<-sled324%>%
  filter(between(date,Launch.18,Retrieval.20))%>%
  ggplot(aes(x=date,y=SalinityInSitu_2pCal_wOffset,color=TempInSitu))+
  geom_line()
f.plot<-sen337%>%
  filter(between(date,Launch.18,Retrieval.20))%>%
  ggplot(aes(x=date,y=SalinityInSitu_2pCal,color=TempInSitu))+
  geom_line()
g.plot<-sen343%>%
  filter(between(date,Launch.18,Retrieval.20))%>%
  ggplot(aes(x=date,y=SalinityInSitu_2pCal_wOffset,color=TempInSitu))+
  geom_line()
h.plot<-sen331%>%
  filter(between(date,Launch.18,Retrieval.20))%>%
  ggplot(aes(x=date,y=SalinityInSitu_2pCal,color=TempInSitu))+
  geom_line()

ggdraw() +
  draw_plot(e.plot, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(f.plot, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(g.plot, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(h.plot, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("sled324", "sn337", "sn343", "sn331"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5)) +
  ggsave(paste0('Output/Daily_CT_Profiles/0118-2021_CT_profiling_',date,'.pdf'), width = 15, height = 12)


