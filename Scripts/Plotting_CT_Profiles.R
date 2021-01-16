## 1/15/2021 CT files
## File names for ggsave need to be changed before running script

rm(list=ls())
library(tidyverse)
library(cowplot)

pump<-read_csv('Data/Cond_temp/Calibrated_files/fixed_cal/011521_CT354_1pcal.csv')
boat<-read_csv('Data/Cond_temp/Calibrated_files/fixed_cal/011521_CT316_1pcal.csv')
insitu<-read_csv('Data/Cond_temp/Calibrated_files/fixed_cal/011521_CT318_1pcal_stable_insitu.csv')
solution<-read_csv('Data/Cond_temp/Calibrated_files/fixed_cal/011521_CT319_1pcal_in50_allday.csv')

## Plotting
Launch<-'2021-01-15 08:48:25'
Retrieval<-'2021-01-15 14:51:05'
Launch<-Launch %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)
Retrieval<-Retrieval %>% parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),locale = default_locale(), trim_ws = TRUE)

## Plotting Salinities
p.plot<-pump%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()
b.plot<-boat%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>27)%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()
i.plot<-insitu%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()
s.plot<-solution%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()

ggdraw() +
  draw_plot(p.plot, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(b.plot, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(i.plot, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(s.plot, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5))

## Plotting E_Conductivity
p.plot<-pump%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=E_Conductivity))+
  geom_line()
b.plot<-boat%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>27)%>%
  ggplot(aes(x=date,y=E_Conductivity))+
  geom_line()
i.plot<-insitu%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=E_Conductivity))+
  geom_line()
s.plot<-solution%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=E_Conductivity))+
  geom_line()

ggdraw() +
  draw_plot(p.plot, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(b.plot, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(i.plot, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(s.plot, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5))

## Plotting Sp_Conductance
p.plot<-pump%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=Sp_Conductance.1pcal))+
  geom_line()
b.plot<-boat%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>27)%>%
  ggplot(aes(x=date,y=Sp_Conductance.1pcal))+
  geom_line()
i.plot<-insitu%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=Sp_Conductance.1pcal))+
  geom_line()
s.plot<-solution%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=Sp_Conductance.1pcal))+
  geom_line()

ggdraw() +
  draw_plot(p.plot, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(b.plot, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(i.plot, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(s.plot, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5))


## Plotting Temperature
p.plot<-pump%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()
b.plot<-boat%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>27)%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()
i.plot<-insitu%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()
s.plot<-solution%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()

ggdraw() +
  draw_plot(p.plot, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(b.plot, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(i.plot, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(s.plot, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5))

## Plotting Pump CT
Sal<-pump%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()
Econ<-pump%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>27)%>%
  ggplot(aes(x=date,y=E_Conductivity))+
  geom_line()
Scon<-pump%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=Sp_Conductance.1pcal))+
  geom_line()
Temp<-pump%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()

ggdraw() +
  draw_plot(Sal, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(Econ, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(Temp, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(Scon, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5)) +
  ggsave('Output/Daily_CT_Profiles/011521_354_pump.png', width = 15, height = 12)

## Plotting Boat CT
Sal<-boat%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(SalinityInSitu_1pCal>27)%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()
Econ<-boat%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(E_Conductivity>35000)%>%
  ggplot(aes(x=date,y=E_Conductivity))+
  geom_line()
Scon<-boat%>%
  filter(between(date,Launch,Retrieval))%>%
  filter(Sp_Conductance.1pcal>50000)%>%
  ggplot(aes(x=date,y=Sp_Conductance.1pcal))+
  geom_line()
Temp<-boat%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()

ggdraw() +
  draw_plot(Sal, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(Econ, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(Temp, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(Scon, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5)) +
  ggsave('Output/Daily_CT_Profiles/011521_316_boat.png', width = 15, height = 12)

## Plotting Stable in situ CT
Sal<-insitu%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()
Econ<-insitu%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=E_Conductivity))+
  geom_line()
Scon<-insitu%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=Sp_Conductance.1pcal))+
  geom_line()
Temp<-insitu%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()

ggdraw() +
  draw_plot(Sal, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(Econ, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(Temp, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(Scon, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5)) +
  ggsave('Output/Daily_CT_Profiles/011521_318_stable_insitu.png', width = 15, height = 12)

## Plotting only calibration CT
Sal<-solution%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=SalinityInSitu_1pCal))+
  geom_line()
Econ<-solution%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=E_Conductivity))+
  geom_line()
Scon<-solution%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=Sp_Conductance.1pcal))+
  geom_line()
Temp<-solution%>%
  filter(between(date,Launch,Retrieval))%>%
  ggplot(aes(x=date,y=TempInSitu))+
  geom_line()

ggdraw() +
  draw_plot(Sal, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(Econ, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(Temp, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(Scon, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.5, 0, 0.5), y = c(1, 1, 0.5, 0.5)) +
  ggsave('Output/Daily_CT_Profiles/011521_319_in50_allday.png', width = 15, height = 12)


