#####################################################################
## Hyo Byun
## Mayberg Lab - Emory University
## 11-2012
#####################################################################

###################################################
# haverDist Formula, distance between two coordinates
EarthRad=6371000 #Radius of Earth in meters
haverDist<-function(aLong,aLat,bLong,bLat){
  dLat=2*pi*(bLat-aLat)/360.0; dLon=2*pi*(bLong-aLong)/360.0
  a=(sin(dLat/2))^2+cos(2*pi*aLat/360)*cos(2*pi*bLat/360)*(sin(dLon/2)^2)
  return(EarthRad*2*atan2(sqrt(a),sqrt(1-a)))
}


###################################################
#Calculate Distance Traveled from i-1 to i
calcDistTraveled <- function(lat, lon) {
	cat("Calculating instantaneous distance traveled\n")
    distTraveled <- rep(0, length(lat))
    for (i in 2:length(lat)) {
      distTraveled[i]<-haverDist(lon[i],lat[i],lon[i-1],lat[i-1])
      print(lon[i])
    }
    return(distTraveled)
}

###################################################
calcSumDistance <- function(distTraveled) {
	cat("Calculating distance sum\n")
  distTraveledSum <- rep(0, length(distTraveled))
  for(i in 2:length(distTraveled)) {
    distTraveledSum[i]<-(distTraveled[i]+distTraveledSum[i-1])
  }
  return(distTraveledSum)
}

###################################################
#Time Differences from point i-1 to i
calcTimeDiff <- function(time) {
	cat("Calculating time differences\n")
  timeDiff <- rep(0, length(time))
  for (i in 2:length(timeDiff)) {
    timeDiff[i]<-difftime(time[i], time[i-1],units="secs")
  }
  cat("\n")
  return(timeDiff)
}


###################################################
#Velocity in m/s from point i-1 to i
calcVelocity <- function(distTraveled,timeDiff) {
	cat("Calculating instantaneous velocities\n")
  velocity <- rep(0, length(distTraveled))
  for (i in 2:length(velocity)) {
    velocity[i]<- distTraveled[i]/timeDiff[i]
  }
  return(velocity)
}

###################################################
#Convert Velocity form m/s to mph
velocityMIpH <- function(velocityMpS) {
	cat("Calculating velocity converstion to mph\n")
  velocityMIpH <- rep(0, length(velocityMpS))
  velocityMIpH<- velocityMpS*2.2369362920544
  return(velocityMIpH)
}


###################################################
#Calc Acceleration in m/ss
calcAccl <- function(velocity, timeDiff) {
	cat("Calculating instantaneous acceleration\n")
  velocityDelt <- rep(0, length(velocity))
  for (i in 2:length(velocityDelt)) {
    velocityDelt[i]=velocity[i]-velocity[i-1];
  }
  accl <- rep(0, length(velocityDelt))
  for (i in 2:length(accl)) {
    accl[i]=velocityDelt[i]/timeDiff[i]
  }
  return(accl)
}

###################################################
#Mollweide equal area projection
calcMollweideProjection<- function(lat,lon) {
	cat("Calculating equal area mollweide projection\n")
    temp<-mapproject(lon,lat,projection="mollweide")
    coords.df <- data.frame("moll_x"=temp$x,"moll_y"=temp$y)
    gpspoints.df$moll_x<- coords.df$moll_x
    gpspoints.df$moll_y<- coords.df$moll_y
    return(gpspoints.df)
}


###################################################
# Get the number of days with missing pedometer data
getMissingDays <-function(dailySteps) {
  missingDays<-0
  for(i in 1:length(dailySteps)) {
    if(dailySteps[i]==0)
      missingDays<-missingDays+1
  }
  return(missingDays)
}

###################################################
# Get the number of average Steps per day, Excluding days missing days
getAverageStepsPerDay<- function(dailySteps) {
  missingDays<-getMissingDays(dailySteps)
  
  if(missingDays==length(dailySteps))
    return(NA) #There is no data
  else
    return(sum(dailySteps)/(length(dailySteps)-missingDays))
}

###################################################
# Average Meters Per Day
getAverageMetersPerDay<- function(dailyMeters) {
  missingDays<-getMissingDays(dailyMeters)
  
  if(missingDays==length(dailyMeters))
    return(NA) #There is no data
  else
    return(sum(dailyMeters)/(length(dailyMeters)-missingDays))
}

###################################################
#Get the number of  Steps per day
# pedometer = "fitbit" or "omron"
outputStepsPerDay<- function(startDay,stepSum,pedometer) {
  if(pedometer=="fitbit")
    dailyN=288;
  if(pedometer=="omron")
    dailyN=24;
  
	cat("Calculating steps per day\n")
  startDay<-as.POSIXlt(startDay) ## convert string of date into usable date format
  dailySteps<-NA
  n<-length(stepSum)
  i<-1
  while(i*dailyN<=n) {
    if(i==1)
      dailySteps[i]<-stepSum[i*dailyN]
    else
      dailySteps[i]<-stepSum[i*dailyN]-stepSum[i*dailyN-dailyN]
   # cat(paste("  Steps in",startDay, " :",dailySteps[i]),"\n")
    startDay=startDay+(days(1)) #increment day
    i=i+1
  }
  return(dailySteps)
}



###################################################
# Active Ratio- non 0 steps
calcActiveRatio<- function(steps,missingDays,pedometer) {
  if(pedometer=="fitbit")
    dailyN=288;
  if(pedometer=="omron")
    dailyN=24;
  
  i<-1
  activeIntervals<-0
  while(i<=length(steps)) {
    if(!is.na(steps[i]) && steps[i]>0)
      activeIntervals<-activeIntervals+1
    i<- i+1
  }
  return(activeIntervals/length(steps))
}

###################################################
# Get the average Velocity per Day
getAverageVeloctyPerDay<- function(startDay,endDay, gpsPoints) {
	cat("Calculating average velocity per day\n")
  
  tempDay<-as.POSIXlt(startDay)
  endDay<-as.POSIXlt(endDay)
  aveDailyVelocity<-NA
  n<-nrow(gpsPoints)
  i<-1
  theDay<-subset(gpsPoints, as.POSIXlt(gpsPoints$time)>=tempDay)
  theDay<-subset(theDay, as.POSIXlt(theDay$time)<(tempDay+days(1)))
  while(tempDay<endDay) {
    if(is.nan(mean(theDay$velocityMIpH)))
      velAve<-0
    else
      velAve<-mean(theDay$velocityMIpH)
    aveDailyVelocity[i]<-velAve
    tempDay=tempDay+(days(1))
    theDay<-subset(gpsPoints, as.POSIXlt(gpsPoints$time)>=tempDay)
    theDay<-subset(theDay, as.POSIXlt(theDay$time)<(tempDay+days(1)))
    i<-i+1
  }
  return((aveDailyVelocity))
}


###################################################
# Get Meters Per Day
getMeteresPerDay<- function(startDay,endDay, gpsPoints) {
  cat("Calculating meters per day traveled (gps)\n")
  
  tempDay<-as.POSIXlt(startDay)
  endDay<-as.POSIXlt(endDay)
  dailyMeters<-NA
  n<-nrow(gpsPoints)
  i<-1
  clipFuture<-subset(gpsPoints, as.POSIXlt(gpsPoints$time)< (tempDay+days(1)))
  nRowsInTimeInterval<-nrow(subset(clipFuture, as.POSIXlt(clipFuture$time)>=tempDay))
  iStart<- nrow(clipFuture)-nRowsInTimeInterval
  iEnd<-nrow(clipFuture) 
  while(tempDay<endDay) {
    cat(paste("Meters/day for day ", tempDay , "\n"))
    if((iEnd-iStart)==0 || (iEnd)==0)
      dailyMeter<-0
    else if(iStart==0)
      dailyMeter<-gpsPoints$calcSumDistance[iEnd]
    else  
      dailyMeter<-gpsPoints$calcSumDistance[iEnd]-gpsPoints$calcSumDistance[iStart]
    
    dailyMeters[i]<-dailyMeter
    #  print(tempDay)
    tempDay=tempDay+(days(1))
    
    clipFuture<-subset(gpsPoints, as.POSIXlt(gpsPoints$time)<(tempDay+days(1)))
    nRowsInTimeInterval<-nrow(subset(clipFuture, as.POSIXlt(clipFuture$time)>=tempDay))
    iStart<- nrow(clipFuture)-nRowsInTimeInterval
    iEnd<-nrow(clipFuture) 
    
    
    i<-i+1
  }
  return((dailyMeters))
}

###################################################
# split period into days
getDays<- function(startDay,endDay) {
	i=0
	startDay<-as.POSIXlt(startDay)
	endDay<-as.POSIXlt(endDay)
	days<-NA
	while(startDay<endDay) {
		days[i]<- paste("",startDay,sep="")
		# cat(paste("  Steps in",startDay, " :",dailySteps[i]),"\n")
		startDay=startDay+(days(1)) #increment day
		i=i+1
	}
	days[i]<- paste("",startDay,sep="")
	return(days)
}

###################################################
# Pixels Covered
calcPixelsCovered<- function(moll_x, moll_y) {
	
	##########################
	##Plot Area Covered ##lon , ##lat
	png(paste(dir,"output/rawpixelmap.png",sep=""),width=5000,height=5000,antialias="none")
	
	par(mar=c(0,0,0,0))   
	limits<-mapproject(c(-85.0,-83.0),c(33,35.0),projection="mollweide")
	plot(limits$x,limits$y,axis=FALSE)	
	par(new=TRUE)
	plot(moll_x,moll_y, xlim=limits$x, ylim=limits$y,pch=20,axis=FALSE,cex=2)	
	
	dev.off()
	
	cat("Running pixel counter (java)\n")
	system(paste(dir,"bin",sep=""),intern=TRUE)
	setwd(paste(dir,"bin",sep=""))
	
	areaCovered<-as.integer(system(paste("java ","pixelCounter",sep=""),intern=TRUE,wait=TRUE))
	setwd("~/")
	return(areaCovered)
}

# walkingVsDrivingRatio<-function(fitbit.df, velocity) {
#   fitbit.df<-fitbitpoints.df
#   fiveMinGPS.df <- data.frame("time"=fitbit.df$time,"fiveMinute"=fitbit.df$fiveMinute,"maxVelocMPH"=NA)
#   subset(velocity, velocity)
#   tempSpeeds<-NA
#   for(i in 1:nrow(fiveMinGPS.df)) {
#     toTime<-as.POSIXlt(fiveMinGPS.df$time[1])
#   }
#   test<-subset(gpspoints.df, as.POSIXlt(gpspoints.df$time)>as.POSIXlt("2012-09-05 02:15:07")
# }
