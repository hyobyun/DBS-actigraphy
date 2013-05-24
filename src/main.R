#####################################################################
# Hyo Byun
# Mayberg Lab - Emory University
# 11-2012
#####################################################################
options(warn=-1)
rm(list=ls())
graphics.off()
sink() 
args<-commandArgs(TRUE)

#####################################################################
# Take console input
cat("--------------------------------------------------------\n")
cat("DBS GPS/PEDOMETER ANALYSIS                              \n")
cat("Emory University                                        \n")
cat("Mayberg Lab                                             \n")
cat("--------------------------------------------------------\n")




cat("Please Enter Subject (Format: DBS###) \n:")
subj <- readLines(file("stdin"),1)

cat("\nPlease Enter Timepoint (preop/postop)  \n:")
timepoint <- readLines(file("stdin"),1)

cat("\nPlease Enter Start Date (Inclusive) (Format: YYYY-MM-DD )  \n:")
dateStart <- readLines(file("stdin"),1)

cat("\nPlease Enter End Date (Inclusive) (Format: YYYY-MM-DD)  \n:")
dateEnd <- readLines(file("stdin"),1)

cat("--------------------------------------------------------\n")

#subj      = "DBS036"
#dateStart = "2013-02-08" #Inclusive
#dateEnd   = "2013-02-27" #Exclusive

save      = TRUE 
verbose   = FALSE

dir<-"G:/MaybergShare/DBS_GPS_Pedometer/"


dir.create(paste(dir, "output/",subj,sep=""))
dir.create(paste(dir, "output/",subj,"/",timepoint,sep=""))

#####################################################################
# Header
# Import files and get dependencies

source(paste(dir,"src/analysisFunctions.R",sep=""))
source(paste(dir,"src/plotFunctions.R", sep=""))
source(paste(dir,"src/parseFunctions.R", sep=""))

library(XML)
library(png)
library(RgoogleMaps)
library(maps)
library(mapproj)
library(lubridate)
library(R.cache)
library(geosphere)

# Set cache directory
setCacheRootPath(path=paste(dir,"cache", sep=""))

# warnings shut up
options(warn=-1)

#sink(paste(dir, "output.txt",sep=""),append=TRUE)

#Function parameters need end date to be exclusive
dateEnd<-as.POSIXlt(dateEnd)
cat("HERE\n")
dateEnd<-as.character(dateEnd+(days(1)) )


#####################################################################
# Parse Raw Data Files

# Parse QStarz GPS Data
cat("Parsing QStarz GPS\n")
gpspoints_qstarz.df <-getQStarzGPSPoints(dateStart,dateEnd)

# Parse Magellan GPS Data
cat("Parsing Magellan GPS\n")
gpspoints_magellan.df <-getMagellanGPSPoints(dateStart,dateEnd)

# Merge GPS Data
gpspoints.df<-rbind(gpspoints_qstarz.df,gpspoints_magellan.df)

# Parse Omron Pedometer
cat("Parsing Omron Pedometer\n")
pedometerdata_omron.df <-getOmronSteps(dateStart,dateEnd)

# Parse Fitbit Pedometer
cat("Parsing Fitbit Pedometer \n")
pedometerdata_fitbit.df <-getFitBitSteps(dateStart,dateEnd)

cat("--------------------------------------------------------\n")


######################################################################
# Perform GPS Calculations
gpspoints.df$lat<-as.numeric(as.character(gpspoints.df$lat))
gpspoints.df$lon<-as.numeric(as.character(gpspoints.df$lon))
# Distance Traveled for every 2 consecutive points
gpspoints.df$distTraveled<-calcDistTraveled(gpspoints.df$lat,gpspoints.df$lon)

# Sum up Distances
gpspoints.df<- subset(gpspoints.df, gpspoints.df$distTraveled<1000) # Filter out large gaps
gpspoints.df$calcSumDistance<-calcSumDistance(gpspoints.df$distTraveled)

# Time differences for every 2 consecutive points
gpspoints.df$timeDiff<-calcTimeDiff(gpspoints.df$time)

# velocity for every 2 consecutive points in m/s
gpspoints.df$velocityMpS <- calcVelocity(gpspoints.df$distTraveled, gpspoints.df$timeDiff)

# velocity for every 2 consecutive points in mph
gpspoints.df$velocityMIpH <- velocityMIpH(gpspoints.df$velocityMpS)

# accl for every 2 consecutive points
gpspoints.df$acclMpSS <- calcAccl(gpspoints.df$velocityMpS, gpspoints.df$timeDiff)

# Calculate Mollweide Projection Coordinates
gpspoints.df<-calcMollweideProjection(gpspoints.df$lat,gpspoints.df$lon)

# Sum up distance traveled per day (gps)
metersPerDay<- getMeteresPerDay(dateStart,dateEnd,gpspoints.df)

# Get 0 distance Days
zeroDistanceDays<-getMissingDays(metersPerDay)

#Calculate pixels covered
areaCovered<--1
if(Sys.info()['sysname'] != 'Windows')
  areaCovered<- calcPixelsCovered(gpspoints.df$moll_x,gpspoints.df$moll_y)



######################################################################
# Perform Pedometer Calculations

# Sum up Steps (Omron)
pedometerdata_omron.df$SumSteps<-calcSumDistance(pedometerdata_omron.df$totalSteps)

# Sum up Steps (fitbit)
pedometerdata_fitbit.df$SumSteps<-calcSumDistance(pedometerdata_fitbit.df$steps)

# Choose a pedometer to use- Use the one with more data
if(pedometerdata_omron.df[nrow(pedometerdata_omron.df),]$SumSteps > pedometerdata_fitbit.df[nrow(pedometerdata_fitbit.df),]$SumSteps) {
  cat("Chosen Omron \n")
  pedometerType="omron"
  if( pedometerdata_fitbit.df[nrow(pedometerdata_fitbit.df),]$SumSteps != 0) {
    cat("WARNING: Omron data non zero")
  }
  pedometerData.df<-pedometerdata_omron.df
} else {
  cat("Chosen Fitbit \n")
  pedometerType="fitbit"
  if(pedometerdata_omron.df[nrow(pedometerdata_omron.df),]$SumSteps != 0) {
    cat("WARNING: Omron data non zero")
  }
  pedometerData.df<-pedometerdata_fitbit.df
}

# Sum up steps per day
stepsPerDay<-outputStepsPerDay(dateStart,pedometerData.df$SumSteps,pedometerType)

# Take average steps per day - Empty Days IGNORED
averageStepsPerDay<-getAverageStepsPerDay(stepsPerDay)

#Count number of missing Days 
PedoMissingDays<-getMissingDays(stepsPerDay)

# Active Ratio
activeRatio<-calcActiveRatio(pedometerData.df$steps,PedoMissingDays,pedometerType)

# Get average meters per day, excluding empty days
averageMetersPerDay<-getAverageMetersPerDay(metersPerDay)

# Get period split into days
days <- getDays(dateStart,dateEnd)
temp <- data.frame("date"=days, "steps per day"= stepsPerDay, "meters per day" = metersPerDay)

# Save Steps per day
if(save)
  write.table(temp,file=paste(dir, "output/",subj,"/",timepoint, "/stepsAndMetersPerDay.csv",sep="") ,sep=",",row.names=F)

######################################################################
# FILTER HERE
cat("Post Analysis Filtering\n")
gpspoints.df<- subset(gpspoints.df, gpspoints.df$velocityMIpH<100)
gpspoints.df<- subset(gpspoints.df, abs(gpspoints.df$acclMpSS)<12)

gpspoints.df<- subset(gpspoints.df, gpspoints.df$lat<=360)
gpspoints.df<- subset(gpspoints.df, gpspoints.df$lon<=360)
gpspoints.df<- subset(gpspoints.df, gpspoints.df$lat>=-360)
gpspoints.df<- subset(gpspoints.df, gpspoints.df$lon>=-360)
merged.df<-merge(gpspoints.df, pedometerData.df, by="time", all.x=TRUE, all.y=TRUE)

######################################################################
# Post Filter Calculations
# Calculate minimum convex polygon
cat("Calculating minimum convex polygon\n");
minConvexPolyPoints <-chull(gpspoints.df$lon, gpspoints.df$lat)
minConvexPolygon <- areaPolygon(data.frame(gpspoints.df$lon[minConvexPolyPoints],gpspoints.df$lat[minConvexPolyPoints]))/1000000

######################################################################
# GRAPHS 
cat("--------------------------------------------------------\n")
cat("Creating Graphs\n")

velocityHistogram(save, merged.df$velocityMIpH)
accelerationHistogram(save, merged.df$acclMpSS)
convexHullGraph(save, gpspoints.df$lat,gpspoints.df$lon,minConvexPolyPoints)
#googleGPSmapraw(save,gpspoints.df$lat,gpspoints.df$lon, gpspoints.df$velocityMIpH)
googleGPSmap(save,gpspoints.df$lat,gpspoints.df$lon, gpspoints.df$velocityMIpH)
#velocStepsPlot(save,merged.df$time, merged.df$velocityMIpH, merged.df$steps, dateStart,dateEnd)
#stepsMetersBarPlot(save,stepsPerDay,metersPerDay,merged.df$time[1],merged.df$time[nrow(merged.df)])


out_StepsTaken<- pedometerData.df$SumSteps[nrow(pedometerData.df)]
out_AverageStepsPerDay<-averageStepsPerDay
out_ActiveRatio <-activeRatio
out_MissingPedometerDays <- PedoMissingDays
pedometer<-pedometerType

#############################################################################

## Subtract dateEnd
dateEnd<-as.POSIXlt(dateEnd)
dateEnd<-as.character(dateEnd-(days(1)) )


if(save) {
  # Output to file
  cat("Outputting to csv\n")
  sink(paste(dir, "output/output.csv",sep=""),append=TRUE)
  
  cat(subj, " , ")
  cat(dateStart, "-", dateEnd, ", ")
  cat(pedometer, "  , ")
  cat(timepoint, "  , ")
  cat(format( gpspoints.df$calcSumDistance[nrow(gpspoints.df)]      , digits=7  , width=15  ), ", ")
  cat(format( out_StepsTaken                                        , digits=1  , width=10  ), ", ")
  cat(format( averageMetersPerDay                                   , digits=7  , width=17  ), ", ")
  cat(format( out_AverageStepsPerDay                                , digits=2  , width=16  ), ", ")
  cat(format( out_ActiveRatio                                       , digits=7  , width=11  ), ", ")
  cat(format( areaCovered                                           , digits=7  , width=20  ), ", ")
  cat(format( minConvexPolygon                                      , digits=7  , width=33  ), ", ")
  cat(format( out_MissingPedometerDays                              ,             width=22  ), ", ")
  cat(format( zeroDistanceDays                                      ,             width=15  ), "\n")
  
} else {
  # Output to Console
  cat("\n\n\n")
  cat("--------------------------------------------------------\n")
  cat("Subject: ", subj,"\n")
  cat("Output for Dates: [", dateStart, "-", dateEnd, ")\n")
  cat("--------------------------------------------------------\n")
  
  cat("Pedometer Used                     :",  pedometer                                               , "\n")
  cat("Distance Traveled (meters)         :",  gpspoints.df$calcSumDistance[nrow(gpspoints.df)]        , "\n")
  cat("Steps Taken                        :",  out_StepsTaken                                          , "\n")
  cat("Average Meters Per Day             :",  averageMetersPerDay                                     , "\n")
  cat("Average Steps Per Day              :",  out_AverageStepsPerDay                                  , "\n")
  cat("Active Ratio                       :",  out_ActiveRatio                                         , "\n")
  cat("Pixels Covered                     :",  areaCovered                                             , "\n")
  cat("Minimum Convex Polygon Area (km^2) :",  minConvexPolygon                                        , "\n")
  cat("0 Pedometer Days                   :",  out_MissingPedometerDays                                , "\n")
  cat("0 GPS Days                         :",  zeroDistanceDays                                        , "\n")
}
sink()



cat("--------------------------------------------------------\n")
cat("Analysis Successful :) \n")




#############################################################################
# MISC OLD
#movingave<-ma(merged.df$velocityMIpH)
#matlines(merged.df$time, movingave, type='l',pch=15,cex=1, col='#8B0000',axes=FALSE, ylab="", xlab="", lwd=2)


#library(rgl)
#plot3d(raw.df$lon,raw.df$lat, raw.df$ele, col="red", size=3)
#cols<-rgb(1,1-points.df$v/100,1-points.df$v/100)


#plot(points.df$lon,points.df$lat, col=cols, lwd=1,pch=16,cex=1)
#plot(raw.df$lon,raw.df$lat, type="l", col=colour)
#write.table(points.df,file="gpsout.csv", sep=",",col.names=TRUE,row.names=FALSE)
#plot(points.df$time,points.df$v)\

#temp<- data.frame(Id=(1:length(minConvexPolyPoints)),lon=gpspoints.df$lon[minConvexPolyPoints], lat=gpspoints.df$lat[minConvexPolyPoints])
#temp2<- data.frame(Id=(1:length(minConvexPolyPoints)), Name=(1:length(minConvexPolyPoints)))
#shapeFile <- convert.to.shapefile(temp, temp2, "Id", 1)
#write.shapefile(shapeFile, paste(dir, "output/shapefile",sep=""), arcgis=T)
#shp=importShapefile(paste(dir, "output/shapefile",sep=""),projection="LL");
#googleGPSmapHull(save,gpspoints.df$lat,gpspoints.df$lon, gpspoints.df$velocityMIpH,minConvexPolyPoints)

#test
#test2
#the branch continues
