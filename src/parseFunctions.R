#####################################################################
## Hyo Byun
## Mayberg Lab - Emory University
## 11-2012
#####################################################################





###################################################
# Parse Q-Starz GPS Files
getQStarzGPSPoints <-function(dateStart,dateEnd) {
  cat("--------------------------------------------------------\n")
  cat("Parsing QStarz GPS \n")
  
  ## Prepare dataframes
  RawgpsData.df<-data.frame()
  GPSpoints.df <- data.frame(time = character(), ele=numeric(), lat = numeric(), lon = numeric())
  
  ## Loop through all gps files
  for(filename in list.files(paste(dir,"dataDump/gps_qstarz/",sep=""),include.dirs=FALSE)) { 
    # For some reason, folder name is parsed-skip this
    if(filename=="old")
      next
    
    cat(paste("Parsing gps file ", filename,"\n" ))
    RawgpsData.df<-rbind(RawgpsData.df,read.csv(paste(dir,"dataDump/gps_qstarz/", filename,sep="") ) )
    
    
    ## Convert to correct Time Format
    rawTime<- paste(RawgpsData.df$LOCAL.DATE, RawgpsData.df$LOCAL.TIME)
    time <-strptime(rawTime, "%Y/%m/%d %X")
    
    ## Filter out correct time range
    temp.df <- data.frame(time = time, ele=RawgpsData.df$HEIGHT, lat = RawgpsData.df$LATITUDE, lon =RawgpsData.df$LONGITUDE)
    temp.df<-subset(temp.df,temp.df$time < as.POSIXlt(dateEnd))
    temp.df<-subset(temp.df,temp.df$time >= as.POSIXlt(dateStart))
    
    ##Add to final dataframe
    GPSpoints.df <- rbind(GPSpoints.df, temp.df)
  }
  cat("Finishing gps parsing")
  print(as.POSIXlt(dateStart))
  
  ## Make unique
  GPSpoints.df <- unique(GPSpoints.df)
  
  return(GPSpoints.df)
}


###################################################
#Parse gps files
getMagellanGPSPoints <- function(dateStart, dateEnd) {
  
  cat("--------------------------------------------------------\n")
  cat("Parsing Magellan GPS \n")
  
  ## Do not parse, use preprocessed csv since it will probably be never used again
  
  GPSpoints.df = read.csv(paste(dir,"dataDump/gps_Magellan/magellan_gps_points.csv",sep=""))
  GPSpoints.df<-subset(GPSpoints.df,as.POSIXlt(GPSpoints.df$time) < as.POSIXlt(dateEnd))
  GPSpoints.df<-subset(GPSpoints.df,as.POSIXlt(GPSpoints.df$time) >= as.POSIXlt(dateStart))
  
  
  #  GPSpoints.df <- data.frame(time = character(), ele=numeric(), lat = numeric(), lon = numeric())
  #   
  #   for(filename in list.files(paste(dir,"dataDump/gps_Magellan/",sep=""))) { #look at all files in directory
  #     cat(filename)
  #     fileLocation<-paste(dir,"dataDump/gps_Magellan/", filename,sep="") 
  #     result<-try(doc<-xmlParse(fileLocation,useInternalNodes=TRUE),  silent = TRUE) #try to see if the xml file exists
  #     
  #     top=xmlRoot(doc)
  #     if(class(result)=="try-error") { #file not there
  #       print(paste("WARNING :", filename, "cannot be read"))
  #     } else { #file found, parse
  #       i=4
  #       while (!is.null(top[[1]][[i]][[1]])) {
  #         j=1
  #         while (!is.null(top[[1]][[i]][[j]])) {
  #           try(GPSpoints.df <- rbind(GPSpoints.df,data.frame(time =  strptime(toString.XMLNode(top[[1]][[i]][[j]][[2]][[1]]),  "%Y-%m-%dT%H:%M:%SZ"),ele =  toString.XMLNode(top[[1]][[i]][[j]][[1]][[1]]), lat = as.numeric(xmlAttrs(top[[1]][[i]][[j]])['lat']), lon = as.numeric(xmlAttrs(top[[1]][[i]][[j]])['lon']))),  silent = TRUE)
  #           j=j+1
  #         }
  #         i=i+1
  #       }
  #       
  #       ##SUBSET HERE
  #       GPSpoints.df<-subset(GPSpoints.df,GPSpoints.df$time >= as.POSIXlt(dateStart) & GPSpoints.df$time <= as.POSIXlt(dateEnd))
  #       write.csv(GPSpoints.df,file="test.csv")
  #       ##/SUBSET HERE
  #       
  #       doc=xmlParse(paste(dir,"dataDump/gps/", filename,sep=""),useInternalNodes=TRUE)
  #       print(paste("OK      :", filename, " found and parsed"))
  #     }
  #   }
  return(GPSpoints.df)
}


###################################################
# Parse fitbit files 
## returns data frame of fitbit pedometer information from dateStart to dateEnd
getFitBitSteps <- function(dateStart, dateEnd) {
	print("                                                        ")
	print("Parsing FitBit------------------------------------------")
	fitBitSteps.df <- data.frame(time = character(), fiveMinute = numeric(), steps=numeric())
	dateEnd<- as.POSIXlt(dateEnd)
	datei<-as.POSIXlt(dateStart)
	while(as.numeric(datei)< as.numeric(dateEnd)){  #Go from the initial date to dateEnd inclusive
    print(datei)
		fileName<- paste("fitbit-",datei,".xml", sep="") # assemble file name & location
		fileLocation<-paste(dir,"dataDump/pedometer_fitbit/", fileName,sep="") 
		try(system(paste("sed -i '/./,$!d'", fileLocation),intern=TRUE,ignore.stdout=TRUE),silent=TRUE) #clean whitespace
		result<-try(doc<-xmlTreeParse(fileLocation, handlers=list("comment"=function(x,...){NULL}), asTree = TRUE),  silent = TRUE) #try to see if the xml file exists
		if(class(result)=="try-error") { #Fill with 0
			for (i in 1:288) { #find xml entry for each 5 minute interval
				steps=0
				hours=floor(i/12)
				if(hours<10)
					hours<-paste("0",hours,sep="")
				minutes=(i-floor(i/12)*12)*5
				if(minutes<10)
					minutes<-paste("0",minutes,sep="")
				timeString=paste(datei," ",hours,":",minutes,":00",sep="")
				fitBitSteps.df <- rbind(fitBitSteps.df,data.frame(time=timeString, fiveMinute = i, steps))
			}
			
		} else { #file found, parse
			
			top=xmlRoot(doc)
			for (i in 1:288) { #find xml entry for each 5 minute interval
				steps=as.numeric(toString.XMLNode(top[[23]][[1]][[2]][[1]][[i]][[1]]))
				hours=floor(i/12)
				if(hours<10)
					hours<-paste("0",hours,sep="")
				minutes=(i-floor(i/12)*12)*5
				if(minutes<10)
					minutes<-paste("0",minutes,sep="")
				timeString=paste(datei," ",hours,":",minutes,":00",sep="")
				fitBitSteps.df <- rbind(fitBitSteps.df,data.frame(time=timeString, fiveMinute = i, steps))
			}
			print(paste("OK      :",fileName,"found and parsed"))
		}
		
		datei=datei+(days(1)) #increment day
	}
	return(fitBitSteps.df)
}


###################################################
# Parse omron pedometer csv 
## returns data frame of fitbit pedometer information from dateStart to dateEnd
getOmronSteps <-function(dateStart,dateEnd) {
  cat("--------------------------------------------------------\n")
  cat("Parsing Omron Pedometer\n")
  
  dateEnd<- as.POSIXlt(dateEnd)
  dateStart<-as.POSIXlt(dateStart)
  datei<-dateStart
  omronSteps.df <- data.frame(time=character(), hour = numeric(), steps=numeric(), aerobicSteps=numeric(), totalSteps=numeric())
  omronRaw<-read.csv(paste(dir,"dataDump/pedometer_omron/omron.csv",sep="") ,skip=2)
  omronRaw<-subset(omronRaw, as.POSIXlt(omronRaw$Date, format="%m/%d/%Y")<dateEnd)
  omronRaw<-subset(omronRaw,as.POSIXlt(omronRaw$Date, format="%m/%d/%Y") >=dateStart)
  print(nrow(omronRaw))
  
  #Loop Through Days
  while(datei<dateEnd) {
    
    cat(paste("Parsing pedometer data for day ", datei, "\n"))
    tempRow<-subset(omronRaw,as.POSIXlt(omronRaw$Date, format="%m/%d/%Y") == datei)
    #Parse only if entry Exists
      tempSteps<-subset(tempRow,select=c(8:31))
      tempAerobicSteps<-subset(tempRow,select=c(32:55))
      for(i in 0:23) {
        if(i<10)
          hour<-paste("0",i,sep="")
        else
          hour<-i
        date<-paste(datei," ",hour,":00:00",sep="")
        if(sum(tempRow$Total.Steps)>0) {
          omronSteps.df <-rbind(omronSteps.df,data.frame(time=date,hour=i,steps=as.numeric(tempRow[8+i][1]),aerobicSteps=as.numeric(tempRow[32+i][1]),totalSteps=(as.numeric(tempRow[8+i][1])+as.numeric(tempRow[32+i][1]))))
        } else {
          omronSteps.df <-rbind(omronSteps.df,data.frame(time=date,hour=i,steps=0,aerobicSteps=0,totalSteps=0))
        }
      }
    datei=datei+(days(1)) #increment day
  }
  
  return(omronSteps.df)
}