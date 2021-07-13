### Test comparing autosamplers###
## By Nyssa Silbiger ###
## 7/13/2021 ###


## load libraries ###
library(here)
library(tidyverse)
library(janitor)

### read in data ####

data<-read_csv(here("Data","AutosamplerTest","AutoSamplerCarbChem.csv"))


### process ####
data_sum<-data %>%
  clean_names() %>%
  pivot_longer(cols = c(p_h_cmore,p_h_craig), names_to = "probe", values_to = "pH")%>%
  group_by(hg_cl2,time_in_bag, probe)%>%
  summarise(TA_mean = mean(alk_2),
            TA_SE = sd(alk_2)/sqrt(n()),
            pH_mean = mean(pH),
            pH_SE = sd(pH)/sqrt(n()))

data_sum %>%
  filter(probe=="p_h_cmore")%>%
  ggplot(aes(x = time_in_bag,y = TA_mean, color = hg_cl2))+
  geom_point(position = position_dodge(width = 0.2), size = 3)+
  geom_errorbar(aes(ymin = TA_mean-TA_SE, ymax = TA_mean+TA_SE), width = 0.2,position = position_dodge())+
  ylim(2220,2235)


data_sum %>%
  ggplot(aes(x = time_in_bag,y = pH_mean, color = hg_cl2, shape = probe))+
  geom_point(position = position_dodge(width = 0.3), size = 3)+
  geom_errorbar(aes(ymin = pH_mean-pH_SE, ymax = pH_mean+pH_SE), width = 0.2,position = position_dodge(width = 0.3))+
  ylim(8.15,8.35)
