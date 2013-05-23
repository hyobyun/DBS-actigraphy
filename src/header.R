#####################################################################
## Hyo Byun
## Mayberg Lab - Emory University
## 11-2012
#####################################################################
## header.r
## sets base folders, libraries, and constants
##
#####################################################################
#sink() 
graphics.off()
rm(list=ls())
#graphics.off()
## EDIT HERE --------------------------------------------------------
dir<-"Dropbox/MaybergLab/DBS_GPS_FITBIT/"

##STOP EDIT ---------------------------------------------------------
#####################################################################       
## Includes

source(paste(dir,"src/analysisFunctions.R",sep=""))
source(paste(dir,"src/plotFunctions.R", sep=""))
source(paste(dir,"src/parseFunctions.R", sep=""))

## End Includes
#####################################################################
#check for libs
library(XML)
library(png)
library(RgoogleMaps)
library(maps)
library(mapproj)
library(lubridate)
library(R.cache)
library(geosphere)


##Set cache directory
setCacheRootPath(path=paste(dir,"cache", sep=""))
##warnings shut up
options(warn=-1)

#sink(paste(dir, "output.txt",sep=""),append=TRUE)

##
cat("--------------------------------------------------------\n")
cat("DBS GPS/FITBIT PARSER                                   \n")
cat("Mayberg Lab                                             \n")
cat("--------------------------------------------------------\n")