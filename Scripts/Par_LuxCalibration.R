#### PAR vs LuX calibration 
### Nyssa Silbiger
#### 9/24/2021 ####



######## Load Library ##########
library(tidyverse)
library(here)
library(broom)


### Read in PAR and Lux Data August

PAR<-read_csv(here("Data","August2021","Cabral_Sled","20210808","QC_files","PAR_20210808.csv"))
Lux<-read_csv(here("Data","August2021","Cabral_Sled","20210808","QC_files","QC_Light_20210810_Light_20555866.csv")) %>%
  mutate(date = floor_date(date, unit = "1 min"))


BothLight<-PAR %>%
  left_join(Lux) %>%
  drop_na(Lux,PAR) # remove the missing data


fit <- nls(PAR ~ SSasymp(Lux, yf, y0, log_alpha), data = BothLight)
terms<-tidy(fit)

ggplot(BothLight,aes(x = Lux, y = PAR))+
  geom_point() +
  geom_line(aes(x = Lux, y = fitted(fit)))

# Conversion equation from Lux to PAR for August 2021
Y<-16778.33+(-0.5003277-16778.33)*exp(1)^(-exp( -13.29572)*BothLight$Lux)

### March 2022


PAR<-read_csv(here("Data","March2022","Varari_Sled","20220322","QC_files","PAR_20220322.csv"))%>%
  mutate(date = floor_date(date, unit = "10 min"))%>%
  group_by(date)%>%
  summarise(PAR_mean = mean(PAR, na.rm = TRUE)) %>%
  ungroup()
  
  
Lux<-read_csv(here("Data","March2022","Varari_Sled","20220322","QC_files","QC_Light_20220322_Light_845.csv")) %>%
  mutate(date = floor_date(date, unit = "10 min"))%>%
  group_by(date)%>%
  summarise(Lux_mean = mean(Lux, na.rm = TRUE))%>%
  ungroup()


BothLight<-PAR %>%
  left_join(Lux) %>%
  drop_na(Lux_mean,PAR_mean) # remove the missing data

ggplot(BothLight, aes(x = log(PAR_mean+1), y = log(Lux_mean+1)))+
  geom_point()

ggplot(BothLight, aes(x = date))+
  geom_line(aes(y = PAR_mean), color = "red")+
  geom_line(aes(y = Lux_mean), color = "blue")

fit <- nls(PAR_mean ~ SSasymp(Lux_mean, yf, y0, log_alpha), data = BothLight)
terms<-tidy(fit)



# This equation forces the intercept through 0
Y<-1555.751648 +(0-1555.751648 )*exp(1)^(-exp( -10.777624)*BothLight$Lux)

# average across 10 mins
Y<-6825.544089  +(0-6825.544089  )*exp(1)^(-exp( -12.455219)*BothLight$Lux_mean)


ggplot(BothLight,aes(x = Lux_mean+1, y = PAR_mean+1))+
  geom_point() +
  geom_line(aes(x = Lux_mean+1, y = Y+1), lwd = 3, color = "red")+
  coord_trans(x = "log", y = "log")

