# Cleaned Respo Code for plate community P/R #####
# Created by Nyssa Silbiger
# Edited on 7/22/2021

#Install Packages 

if ("github" %in% rownames(installed.packages()) == 'FALSE') install.packages('github') 
if ("segmented" %in% rownames(installed.packages()) == 'FALSE') install.packages('segmented') 
if ("plotrix" %in% rownames(installed.packages()) == 'FALSE') install.packages('plotrix') 
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
if ("LoLinR" %in% rownames(installed.packages()) == 'FALSE') install_github('colin-olito/LoLinR') 
if ("lubridate" %in% rownames(installed.packages()) == 'FALSE') install.packages('lubridate') 
if ("chron" %in% rownames(installed.packages()) == 'FALSE') install.packages('chron') 
if ("tidyverse" %in% rownames(installed.packages()) == 'FALSE') install.packages('tidyverse') 
if ("here" %in% rownames(installed.packages()) == 'FALSE') install.packages('here') 
if ("patchwork" %in% rownames(installed.packages()) == 'FALSE') install.packages('patchwork') 


#Read in required libraries
##### Include Versions of libraries
library("segmented")
library("plotrix")
library("gridExtra")
library("LoLinR")
library("lubridate")
library("chron")
library('tidyverse')
library('here')
library("patchwork")

# get the file path


#set the path to all of the raw oxygen datasheets
path.p<-here("Data","August2021","CommunityRespoData","RawO2Files") #the location of all your respirometry files

# bring in all of the individual files
file.names<-basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) #list all csv file names in the folder and subfolders

#basename above removes the subdirectory name from the file, re-nale as file.names.full
file.names.full<-list.files(path = path.p, pattern = "csv$", recursive = TRUE) 

#generate a 3 column dataframe with specific column names
# data is in umol.L.sec
Respo.R<- data.frame(matrix(NA, nrow=length(file.names)*2, ncol=4))
colnames(Respo.R) <- c("FileName","Intercept", "umol.L.sec","Temp.C")
#View(Respo.R)

#Load your respiration data file, with all the times, water volumes(mL), algal biomass weight (dry weight) (g)
Sample.Info <- read.csv(file = here("Data","August2021","CommunityRespoData","CommunityRespoMetadata.csv"))
#View(Sample.Info)

##### Make sure times are consistent ####

# make start and stop times real times, so that we can join the data frames
Sample.Info$start.time <- as.POSIXct(Sample.Info$start.time,format="%H:%M:%S", tz = "") #convert time from character to time
Sample.Info$stop.time <- as.POSIXct(Sample.Info$stop.time,format="%H:%M:%S", tz = "") #convert time from character to time

#view(Sample.Info)

###forloop#####
for (i in 1: length(file.names.full)) {
  FRow<-which(Sample.Info$FileName==strsplit(file.names[i], '.csv')) #stringsplit this renames our file
  Respo.Data1 <-read_csv(file.path(path.p, file.names.full[i]), skip = 1) %>%
    select(Time,Value,Temp) %>% # keep only what we need
    mutate(Time = as.POSIXct(Time, format="%H:%M:%S", tz = "")) %>% # covert time
    drop_na() # drop NAs
  
  Respo.Data1 <- Respo.Data1[-c(1:120),] %>% #we want to start at minute 2 to avoid any noise from the start of the trial
       mutate(sec = 1:n())  #create a new column for every second for the regression

    #Get the filename without the .csv
  rename<- sub(".csv","", file.names[i]) 
  
  
  ### plot and export the raw data ####
  p1<- ggplot(Respo.Data1, aes(x = sec, y = Value))+
    geom_point(color = "dodgerblue")+
    labs(
      x = 'Time (seconds)',
      y = expression(paste(' O'[2],' (',mu,'mol/L)')),
      title = "original"
    )
  
  # thin the data by every 20 seconds to speed it up
  Respo.Data.orig<-Respo.Data1#save original unthinned data #there is no thin() anymore, trying to find alternative 
  Respo.Data1 <-  thinData(Respo.Data1 ,by=20)$newData1 #thin data by every 20 points for all the O2 values
  Respo.Data1$sec <- as.numeric(rownames(Respo.Data1 )) #maintain numeric values for time
  Respo.Data1$Temp<-NA # add a new column to fill with the thinned data
  Respo.Data1$Temp <-  thinData(Respo.Data.orig,xy = c(1,3),by=20)$newData1[,2] #thin data by every 20 points for the temp values
 
  p2<- ggplot(Respo.Data1, aes(x = sec, y = Value))+
    geom_point(color = "dodgerblue")+
    labs(
      x = 'Time (seconds)',
      y = expression(paste(' O'[2],' (',mu,'mol/L)')),
      title = "thinned"
    )
  
  ##Olito et al. 2017: It is running a bootstrapping technique and calculating the rate based on density
  #option to add multiple outputs method= c("z", "eq", "pc")
  Regs  <-  rankLocReg(xall=Respo.Data1$sec, yall=Respo.Data1$Value, alpha=0.5, method="pc", verbose=TRUE)  
 
  # Print across two pages so use baseplot to create the pdf
  pdf(paste0(here("Output","August2021","RespoOutput"),"/", rename,"thinning.pdf"))
  
  p1+p2 # use patchwork to bring the raw and thinned data together
  plot(Regs) # plot the results of Regs
  dev.off()
  
  # fill in all the O2 consumption and rate data
  # need clarity on what this is
  Respo.R[i,2:3] <- Regs$allRegs[1,c(4,5)] #inserts slope and intercept in the dataframe
  Respo.R[i,1] <- rename #stores the file name in the Date column
  Respo.R[i,4] <- mean(Respo.Data1$Temp, na.rm=T)  #stores the Temperature from the incubation in the Temp.C column
}  



#export raw data and read back in as a failsafe 
#this allows me to not have to run the for loop again 
write_csv(Respo.R, here("Data","August2021","CommunityRespoData","Respo.R.csv"))  

Respo.R <- read_csv(here("Data","August2021","CommunityRespoData","Respo.R.csv"))

# Calculate Respiration rate

Respo.R<-Respo.R %>%
  drop_na(FileName) %>% # drop NAs
  left_join(Sample.Info) %>% # Join the raw respo calcuations with the metadata
  mutate(umol.sec = umol.L.sec*volume) %>% #Account for chamber volume to convert from umol L-1 s-1 to umol s-1. This standardizes across water volumes (different because of coral size) and removes per Liter
  mutate_if(sapply(., is.character), as.factor) %>% #convert character columns to factors
  mutate(BLANK = as.factor(BLANK)) #make the blank column a factor


#View(Respo.R)


#Account for blank rate by light/Dark and Block (if we do one blank per block)

#View(Respo.R)

Respo.R_Normalized <- Respo.R %>%
  group_by(Light_Dark, BLANK)%>% # also add block here if one blank per block
  summarise(umol.sec = mean(umol.sec, na.rm=TRUE)) %>%
  filter(BLANK ==1)%>% # only keep the actual blanks
  select(Light_Dark, blank.rate = umol.sec) %>% # only keep what we need and rename the blank rate column
  right_join(Respo.R) %>% # join with the respo data %>%
  mutate(umol.sec.corr = umol.sec - blank.rate, # subtract the blank rates from the raw rates
         mmol.gram.hr = 0.001*(umol.sec.corr*3600)/TotalBiomass)  %>% # convert to mmol g hr-1
  filter(BLANK ==0) %>% # remove all the blank data
  select(Date, PlateID, CowTagID, Top_Bottom,Light_Dark, Site, TotalBiomass, mmol.gram.hr) #keep only what we need

#View(Respo.R_Normalized)

# pivot the data so that light and dark have their own column for net P and R
Respo.R_Normalized<- Respo.R_Normalized %>%
  pivot_wider(names_from = Light_Dark, values_from = mmol.gram.hr) %>%
  rename(Respiration = Dark , NetPhoto = Light) %>% # rename the columns
  mutate(Respiration = - Respiration, # Make respiration positive
         GrossPhoto = Respiration + NetPhoto
         )




write_csv(Respo.R_Normalized,here("Data","August2021","CommunityRespoData","PRCommunityRates.csv") ) # export all the uptake rates
#View(Respo.R)



