#Olympus camera can be set to add GPS info to photos.  
#  Note that the camera must hae a location lock when photo is taken for it to be recorded.
#  GPS location is NOT recorded for underwater photos.


library(exifr)
library(dplyr)
library(leaflet)

#would be better to set this up to access our shared photos in Google Drive
#   for now, went to local directory

pathname <- "C:/Users/Megan/Pictures/Moorea-SGD"

files <- list.files(path = pathname, pattern = "*.JPG")

dat <- read_exif(files)

dat2 <- select(dat,
               SourceFile, DateTimeOriginal,
               GPSLongitude, GPSLatitude)
write.csv(dat2, 'Exifdata.csv',
          row.names = F)
