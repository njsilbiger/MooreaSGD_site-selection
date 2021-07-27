########################################################################
### This calculates the pH using the m-cresol spec method with a cuvette: Dickson SOP6a
### NOTE: you MUST use the pHinsi function from seacarb to calculate the in situ pH at in situ temperature during sample collection.
### This script only exports the pH in the lab.
### Created by Dr. Nyssa Silbiger
### Edited on 7/26/2021
#########################################################################

##  All your data will be exported to that folder

#libraries
library(tidyverse)
library(seacarb)
library(here)

## File names -------------------
foldername<-here("Data", "August2021", "CarbonateChemistry") # folder of the day

filename<-'pHSpec_Data.csv' # data

## Temp and Salinity ----------
#Temeperataure pH was run at IN THE LAB
Temperature<-25
Salinity<-34 # note if you have a range of salinities then import a file with all salinities and modify script below 

### Temperature in situ (either in the aquarium or in the field)#####
## If you want to calculate insitu pH then enter TRUE in the statement below and the filename with the temperature files, if not enter FALSE
CalculateInSitu<-TRUE

# Change slope and intercept with each new batch of dye --------------
# Dye created by: Danielle Barnas
# Date created: 7-27-21
dye_intercept<-17.228
dye_slope<--0.5959

##### Dont change anything below here #####

# Read in the Sample IDs
Data<-read_csv(paste0(foldername,'/',filename))

### Run pH Analysis ##################

pHData<-AllData %>%
  mutate(A1_A2 = (A578-A578_nodye-(A730-A730_nodye))/(A434-A434_nodye-(A730-A730_nodye)), #A1/A2
         A1_A2_corr = A1_A2+(dye_intercept+(dye_slope*A1_A2))*0.05,  #correct for the dye for 50 uL
         pH_in_lab = as.numeric(pHspec(S = Salinity, T = TempInSpec, R = A1_A2_corr)),
         daterun = Sys.Date()) 
  drop_na() # remove the empty wells
  
  
  # Calculate Tris data
  Tris<- pHData %>%
    mutate(
      pHtris = as.numeric(tris(S= rep(34.5,nrow(pHData)),T=rep(Temperature,nrow(pHData)))), ## calculate pH of tris)
  pHtris_error = abs(((pHtris-pH_in_lab)/pHtris)*100)) # calculate error from tris
  

# run this block if calculating in situ pH
  if(CalculateInSitu==TRUE){
    pHData<-pHData %>%
      mutate(pHinsitu = pHinsi(pH = pH_in_lab, Tinsi = TempInSitu, Tlab = TempInSpec, pHscale = "T")) # calculate insitu pH
  }
  
  ### Export the data
  ## all the info
  write_table(paste0(foldername,'/',"pH_full_",filename))
  
  
  ## only the pH data with means and SE of triplicates
  if(CalculateInSitu==TRUE){
    pHData %>% 
      select(CowTagID,Tide, Day_Night,Date, pHinsitu)%>% 
      group_by(CowTagID,Tide, Day_Night,Date, pHinsitu)%>%
      summarise(pHmean = mean(pHinsitu, na.rm=TRUE), SE = sd(pHinsitu, na.rm=TRUE)/n())%>%
      write_table(.,paste0(foldername,'/',"pH_simple_",filename))
  }
  
  if(CalculateInSitu==FALSE){
    pHData %>% 
      select(CowTagID,Tide, Day_Night,Date, pH_in_lab)%>% 
      group_by(CowTagID,Tide, Day_Night,Date, pH_in_lab)%>%
      summarise(pHmean = mean(pH_in_lab, na.rm=TRUE), SE = sd(pH_in_lab, na.rm=TRUE)/n())%>%
      write_table(.,paste0(foldername,'/',"pH_simple_",filename))
  }
  