#### PAR vs LuX calibration 
### Nyssa Silbiger
#### 9/24/2021 ####



######## Load Library ##########
library(tidyverse)
library(here)
library(broom)


### Read in PAR and Lux Data

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

# Conversion equation from Lux to PAR
Y<-16778.33+(-0.5003277-16778.33)*exp(1)^(-exp( -13.29572)*BothLight$Lux)


