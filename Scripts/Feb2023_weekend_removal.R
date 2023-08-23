## View depth to see when to remove sled logger dates

# Created by Danielle Barnas
# Created on 2023-08-22


library(tidyverse)
library(here)

depth1 <- read_csv(here("Data", "Feb2023", "Varari_Sled", "2023-02-26", "QC_files","QC_WL_876_2023-02-26.csv"))
depth2 <- read_csv(here("Data", "Feb2023", "Varari_Sled", "2023-03-20", "QC_files","QC_WL_876_2023-03-20.csv"))
depth3 <- read_csv(here("Data", "Feb2023", "Varari_Sled", "2023-04-21", "QC_files","QC_WL_876_2023-04-21.csv"))

# combine all dataframes
depth <- rbind(depth1, depth2, depth3)

# filter out weekend days
clean.depth <- depth %>% 
  mutate(Weekday = weekdays(date))

# find and filter data when moving sled and while sled is away from the seep
out.of.water <- clean.depth %>% 
  filter(Weekday == "Thursday" | Weekday == "Friday" ) %>% 
  filter(between(date, mdy_hm("2/17/2023 12:00"), mdy_hm("2/21/2023 00:00"))) %>% 
  filter(Depth <0.25)
oow2 <- clean.depth %>% 
  filter(between(date, mdy_hm("2/07/2023 14:12"), mdy_hm("2/12/2023 16:20"))) %>% 
  filter(date < mdy_hm("2/9/2023 12:30"))
oow3 <- clean.depth %>% 
  filter(between(date, ymd_hm("2023-02-21 15:00"), ymd_hm("2023-02-26 00:00")))
oow4 <- clean.depth %>% 
  filter(date == ymd_hm("2023-03-24 18:08"))

out.of.water <- rbind(out.of.water, oow2, oow3, oow4)


away.from.seep <- clean.depth %>% 
  anti_join(out.of.water) %>% 
  filter(between(date, mdy_hm("2/17/2023 14:12"), mdy_hm("2/20/2023 16:20")))
afs2 <- clean.depth %>% 
  filter(between(date, mdy_hm("3/3/2023 13:02"), mdy_hm("3/6/2023 16:42")))
afs3 <- clean.depth %>% 
  filter(between(date, mdy_hm("3/10/2023 08:14"), mdy_hm("3/13/2023 16:50")))
afs4 <- clean.depth %>% 
  filter(between(date, mdy_hm("3/24/2023 18:08"),mdy_hm("4/21/2023 00:00")))

away.from.seep <- rbind(away.from.seep, afs2, afs3, afs4)


weekends <- clean.depth %>% 
  filter(Weekday == "Saturday" |
           Weekday == "Sunday") 

anti_logger_data <- full_join(away.from.seep, out.of.water) %>% 
  full_join(weekends)

clean.depth %>% 
  anti_join(anti_logger_data) %>% 

  ggplot(aes(x = date, y = -Depth)) +
  geom_point() +
  theme_bw()

# save anti_join csv to remove dates from other loggers
anti_logger_data <- anti_logger_data %>% 
  select(date)

write_csv(anti_logger_data, here("Data", "Feb2023", "Varari_Sled", "Anti_Join_sled_weekend_data.csv"))



