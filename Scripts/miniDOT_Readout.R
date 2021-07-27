
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(ggpmisc)

########################
# File Names
########################

Serial<-735719 # Logger's serial number ID
filename_cat<-'Cat_01_17-19_21.csv' # concatenated data from miniDOT Logger
Launch<-'2021-01-17 10:24:00' # Maintain date time format "2020-03-04 14:15:00"
Retrieval<-'2021-01-19 10:08:00' # Maintain date time format "2020-03-04 21:30:00"
Date <- 011921 # today's date

#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

# Tidy Concatenation Data
Concat_Data <- read_csv(paste0('Data/miniDOT/',filename_cat),
                        col_names = FALSE,
                        skip=9) #skips first 9 rows containing instrument information
# Split merged cell into column headings
# note that all vectors are character type
Concat_Data <- separate(Concat_Data,"X1", into=c("Unix_Timestamp_second","UTC_Date_Time","date",
                                                 "Battery_volts","TempInSitu","DO_mg_L","DO_Saturation_percent",
                                                 "Q"), sep=",", remove=TRUE)
# Convert date to date and time vector type
Concat_Data$date <- Concat_Data$date %>%
  parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),
                 locale = default_locale(), trim_ws = TRUE)

# Convert character strings to double number type
Concat_Data$Battery_volts <- Concat_Data$Battery_volts %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)
Concat_Data$TempInSitu <- Concat_Data$TempInSitu %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)
Concat_Data$DO_mg_L<- Concat_Data$DO_mg_L %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)
Concat_Data$DO_Saturation_percent <- Concat_Data$DO_Saturation_percent %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)
Concat_Data$Q <- Concat_Data$Q %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)

# Filter data to only include deployment data
Launch <- Launch %>%
  parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),
                 locale = default_locale(), trim_ws = TRUE)
Retrieval <- Retrieval %>%
  parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),
                 locale = default_locale(), trim_ws = TRUE)
Concat_Data <- Concat_Data %>%
  filter(between(date,Launch,Retrieval))
View(Concat_Data)

# Create simple csv file
write_csv(Concat_Data,paste0('Data/miniDOT/',Date,'_miniDOT_',Serial,'.csv'))

# Plot the data
Concat_Data %>% # this is the dataframe
  ggplot(aes(x= date, y= TempInSitu))+   #setup plot with x and y data
  geom_line() #+ #adding lines
Concat_Data %>% # this is the dataframe
  ggplot(aes(x= date, y= DO_mg_L))+   #setup plot with x and y data
  #ggplot(aes(x=PST, y= Temp_degC))+
  geom_line()# + #adding lines
# scale_y_continuous(sec.axis = sec_axis('Temp_degC', name=derive()))
